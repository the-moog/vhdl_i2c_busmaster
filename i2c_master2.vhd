--------------------------------------------------------------------------------
--   HDL CODE IS PROVIDED "AS IS."  SUPPLIED WITHOUT ANY
--   WARRANTY OF ANY KIND, WHETHER EXPRESS OR IMPLIED, INCLUDING BUT NOT
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
--   PARTICULAR PURPOSE, OR NON-INFRINGEMENT. IN NO EVENT SHALL THE AUTHOR
--   BE LIABLE FOR ANY INCIDENTAL, SPECIAL, INDIRECT OR CONSEQUENTIAL
--   DAMAGES, LOST PROFITS OR LOST DATA, HARM TO YOUR EQUIPMENT, COST OF
--   PROCUREMENT OF SUBSTITUTE GOODS, TECHNOLOGY OR SERVICES, ANY CLAIMS
--   BY THIRD PARTIES (INCLUDING BUT NOT LIMITED TO ANY DEFENSE THEREOF),
--   ANY CLAIMS FOR INDEMNITY OR CONTRIBUTION, OR OTHER SIMILAR COSTS.
--
--   Version History
--   Version 1.0 11/01/2012 Scott Larson
--     Initial Public Release
--   Version 2.0 06/20/2014 Scott Larson
--     Added ability to interface with different slaves in the same transaction
--     Corrected ack_error bug where ack_error went 'Z' instead of '1' on error
--     Corrected timing of when ack_error signal clears
--   Version 2.1 10/21/2014 Scott Larson
--     Replaced gated clock with clock enable
--     Adjusted timing of SCL during start and stop conditions
--   Version 2.2 02/05/2015 Scott Larson
--     Corrected small SDA glitch introduced in version 2.1     
--   V2.2 Downloaded from
--    https://eewiki.net/pages/viewpage.action?pageId=10125324

-------------------------------------------------------------------------------
--
--  Title      Modular VHDL peripheral - I2C bus master controller
--             https://github.com/the-moog/vhdl_modular_blocks
--  File       i2c_master2.vhd
--  Author     Scott Larson
--  Author     Jason Morgan 
--  Dependencies     IEEE VHDL 1164, VHDL 2008 syntax
--
--  Copyright  © Jason Morgan 2018
--  License    This work is licensed under a Creative Commons Attribution 4.0 International License.
--             CC-BY, see LICENSE.TXT
--
-------------------------------------------------------------------------------
--
--  Date       17/7/2018
--  Version    1
--
--  ChangeLog
--  =========
--  Version	   By 				Date 		Change
-- 
--  1		   J A Morgan		17/7/18		Initial version
--
-------------------------------------------------------------------------------
--   Version i2c_master2 1.0 17/07/2018 Jason Morgan 
--   Incomplete list of changes from original code
--     Based on V2.2 of previous work by Scott Larson's of EEWIKI (DigiKey)
--     Added support for variable data lengths of between 1 and 16 bits
--     Added compatibility with vhdl modular blocks : https://github.com/the-moog/vhdl_modular_blocks
--     Replaced non-portable code, improved readability
--     Syntactial improvements (work in progress)
--     Added strength stripper to sda to aid test bench ('H' pullups in upper layers)
--     Changed 'ena' to a pulse of at least clk/2 width
--     Data is latched on ena
--     Added 'continue' to create a repeated start condition

--------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.all;
use IEEE.STD_LOGIC_ARITH.all;


/*!
@brief   MODULAR PERIPHERAL: I2C bus master
@details I2C bus master for use with a CPU address/data bus master controller
@bug     Clock stretching feature currently disabled/broken.
*/
ENTITY i2c_master IS
  GENERIC(
    input_clk : INTEGER := 50_000_000; --input clock speed from user logic in Hz
    bus_clk   : INTEGER := 400_000);   --speed the i2c bus (scl) will run at in Hz
  PORT(
    clk       : IN     STD_LOGIC;                    --system clock
    reset_n   : IN     STD_LOGIC;                    --active low reset   
    nbits     : IN     unsigned(3 downto 0);         --Nbits to transfer less one
    ena       : IN     STD_LOGIC;                    --latch in command
    continue  : IN     STD_LOGIC;                    --If '1' at end of transaction, don't send stop
    rep_start : IN     STD_LOGIC;                    --If '1' at start of transaction, do a repeated start
    addr      : IN     STD_LOGIC_VECTOR(6 DOWNTO 0); --address of target slave
    rw        : IN     STD_LOGIC;                    --'0' is write, '1' is read
    data_wr   : IN     STD_LOGIC_VECTOR(15 DOWNTO 0); --data to write to slave
    busy      : OUT    STD_LOGIC;                    --indicates transaction in progress
    data_rd   : OUT    STD_LOGIC_VECTOR(15 DOWNTO 0); --data read from slave
    ack_error : BUFFER STD_LOGIC;                    --flag if improper acknowledge from slave
    sda       : INOUT  STD_LOGIC;                    --serial data output of i2c bus
    scl       : INOUT  STD_LOGIC;                   --serial clock output of i2c bus
    dbg       : OUT    STD_LOGIC);
END i2c_master;

ARCHITECTURE logic OF i2c_master IS
  CONSTANT divider  :  INTEGER := (input_clk/bus_clk)/4; --number of clocks in 1/4 cycle of scl
  TYPE machine IS(ready, start, wait_restart, restart, contxfer, device_addr, slv_ack_cmd, wr, rd, slv_ack_data, mstr_ack, stop); --needed states
  SIGNAL state         : machine;                        --state machine
  SIGNAL data_clk      : STD_LOGIC;                      --data clock for sda
  SIGNAL data_clk_prev : STD_LOGIC;                      --data clock during previous system clock
  SIGNAL scl_clk       : STD_LOGIC;                      --constantly running internal scl
  SIGNAL sda_int       : STD_LOGIC := '1';               --internal sda
  SIGNAL addr_rw       : STD_LOGIC_VECTOR(7 DOWNTO 0);   --latched in address and read/write
  SIGNAL data_tx       : STD_LOGIC_VECTOR(15 DOWNTO 0);   --latched in data to write to slave
  SIGNAL data_rx       : STD_LOGIC_VECTOR(15 DOWNTO 0);   --data received from slave
  SIGNAL bit_cnt       : unsigned(3 downto 0);           --tracks bit number in transaction
  SIGNAL stretch       : STD_LOGIC := '0';               --identifies if slave is stretching scl  
  signal ena_i         : STD_LOGIC;
  signal continue_i    : STD_LOGIC;  
  signal rep_start_i   : STD_LOGIC;  
  signal busy_i        : STD_LOGIC;

BEGIN                             
  
  
  dbg <= '1' when state = contxfer else '0';

  PROCESS(clk, reset_n)
    VARIABLE count  :  INTEGER RANGE 0 TO divider * 4;  --timing for clock generation
  BEGIN
    IF reset_n = '0' THEN                --reset asserted
      stretch <= '0';
      count := 0; 
      data_clk_prev <= '0'; 
    ELSIF falling_edge(clk) THEN
      data_clk_prev <= data_clk;          --store previous value of data clock
      IF count = (divider * 4) - 1 THEN        --end of timing cycle
        count := 0;                       --reset timer
      ELSIF stretch = '0' THEN            --clock stretching from slave not detected
        count := count + 1;               --continue clock generation timing
      END IF;      
      
      if count < divider then  
        scl_clk <= '0';
        data_clk <= '0';
      elsif count < divider * 2 then
        scl_clk <= '0';
        data_clk <= '1';
      elsif count < divider * 3 then
        scl_clk <= '1';                 --release scl
        /*
        IF to_X01(scl) = '0' THEN              --detect if slave is stretching clock
          stretch <= '1';
        ELSE
          stretch <= '0';
        END IF;
        */
        data_clk <= '1';
      else 
        scl_clk <= '1';
        data_clk <= '0';
      end if;
    END IF;
  END PROCESS;

  --state machine and writing to sda during scl low (data_clk rising edge)
  PROCESS(all)
  variable nbits_i : unsigned(nbits'range);
  BEGIN
    IF reset_n = '0' THEN                  --reset asserted
      state <= ready;                      --return to initial state
      busy <= '1';                         --indicate not available
      --scl_ena <= '0';                      --sets scl high impedance
      sda_int <= '1';                      --sets sda high impedance
      ack_error <= '0';                    --clear acknowledge error flag
      bit_cnt <= nbits;                    --restarts data bit counter
      data_rd <= (others => '0');          --clear data read port 
      ena_i <= '0';
      data_tx <= (others => '0');
      data_rx <= (others => '0');
      addr_rw <= (others => '0'); 
      continue_i <= '0';      
      rep_start_i <= '0';
      nbits_i := conv_unsigned(7, nbits_i'length);      
      busy_i <= '0';
    ELSIF falling_edge(clk) THEN 
      
      --Register busy asap
      busy <= busy_i or ena_i;
      
      if ena = '1' and ena_i = '0' then     --Falling edge of ena
        ena_i <= '1'; 
        continue_i <= continue; 
        if continue = '0' then
          rep_start_i <= rep_start;
        end if;
        nbits_i := nbits;              --collect requested transfer size
        addr_rw <= addr & rw;          --collect requested slave address and command
        data_tx <= data_wr;            --collect requested data to write
      end if;
      
      
      IF data_clk = '1' AND data_clk_prev = '0' THEN  --data clock rising edge
        CASE state IS
          WHEN ready =>                      --idle state
            IF ena_i = '1' THEN              --transaction requested
              busy_i <= '1';                   --flag busy
              state <= start;                --go to start bit
              bit_cnt <= conv_unsigned(7, bit_cnt'length);
            ELSE                             --remain idle
              busy <= '0';                   --unflag busy
            END IF;                                                    
            
          WHEN start | restart =>            --start bit of transaction
            busy_i <= '1';                     --resume busy if continuous mode
            sda_int <= addr_rw(conv_integer(bit_cnt));     --set first address bit to bus
            state <= device_addr;                --go to device_addr               
            ena_i <= '0'; 
            
          WHEN wait_restart =>               --Wait for an ena to start the next transaction 
            busy_i <= '0'; 
            
            if ena_i = '1' then                          
              continue_i <= continue;
            
              if rw = '1' then
                data_tx <= data_wr;            --Load new data 
              end if;            
                           
              if rep_start_i then
                state <= restart;              --Insert a repeated start 
                bit_cnt <= conv_unsigned(7, bit_cnt'length);
              else   
                state <= contxfer;             --Continue without a repeated start
                bit_cnt <= conv_unsigned(nbits_i, bit_cnt'length); 
              end if;
            end if;
            
          WHEN contxfer =>                     
            busy_i <= '1';                      --Send a reapeated start
            ena_i <= '0';
            if rw = '0' then
              sda_int <= data_tx(conv_integer(bit_cnt));   --write first bit of data
              state <= wr;
            else
              state <= rd;
              sda_int <= '1';
            end if;  

          WHEN device_addr =>                --device_addr byte of transaction
            IF bit_cnt = 0 THEN              --device_addr transmit finished
              sda_int <= '1';                --release sda for slave acknowledge
              bit_cnt <= nbits_i;            --reset bit counter for "byte" states
              state <= slv_ack_cmd;           --go to slave acknowledge (device_addr)
            ELSE                             --next clock cycle of command state
              bit_cnt <= bit_cnt - 1;        --keep track of transaction bits
              sda_int <= addr_rw(conv_integer(bit_cnt - 1)); --write address/command bit to bus
            END IF;
            
          WHEN slv_ack_cmd =>                 --slave acknowledge bit (command)
            IF addr_rw(0) = '0' THEN          --write command
              sda_int <= data_tx(conv_integer(bit_cnt));   --write first bit of data
              state <= wr;                   --go to write byte
            ELSE                             --read command
              sda_int <= '1';                --release sda from incoming data
              state <= rd;                   --go to read byte
            END IF;
            
          WHEN wr =>                         --write byte of transaction
            IF bit_cnt = 0 THEN              --write byte transmit finished
              sda_int <= '1';                --release sda for slave acknowledge
              bit_cnt <= nbits_i;            --reset bit counter for "byte" states
              state <= slv_ack_data;         --go to slave acknowledge (write)
            ELSE                             --next clock cycle of write state
              bit_cnt <= bit_cnt - 1;        --keep track of transaction bits
              sda_int <= data_tx(conv_integer(bit_cnt - 1)); --write next bit to bus
              state <= wr;                   --continue writing
            END IF;
            
          WHEN rd =>                         --read byte of transaction
            IF bit_cnt = 0 THEN              --read byte receive finished
              IF continue_i = '1' AND addr_rw = addr & rw THEN  --continuing with another read at same address
                sda_int <= '0';              --acknowledge the byte has been received
              ELSE                           --stopping or continuing with a write
                sda_int <= '1';              --send a no-acknowledge (before stop or repeated start)
              END IF;
              bit_cnt <= nbits_i;            --reset bit counter for "byte" states
              data_rd <= data_rx;            --output received data
              state <= mstr_ack;             --go to master acknowledge
            ELSE                             --next clock cycle of read state
              bit_cnt <= bit_cnt - 1;        --keep track of transaction bits
              state <= rd;                   --continue reading
            END IF;
            
          WHEN slv_ack_data =>               --slave acknowledge bit after a write
            IF continue = '1' THEN         --continue transaction
              state <= wait_restart;         --go to repeated start
              rep_start_i <= rep_start;
            ELSE                             --complete transaction  
              state <= stop;                 --go to stop bit
            END IF;
 
          WHEN mstr_ack =>                   --master acknowledge bit after a read
            IF continue = '1' then 
              state <= wait_restart;         --repeated transfer
              rep_start_i <= '0';
            ELSE
              state <= stop;                 --go to stop bit
            END IF;
 
          WHEN stop =>                       --stop bit of transaction
            busy_i <= '0';                     --unflag busy
            state <= ready;                  --go to idle state
        END CASE;
        
      ELSIF data_clk = '0' AND data_clk_prev = '1' THEN  --data clock falling edge
        CASE state IS
          WHEN start | restart =>
              ack_error <= '0';                     --reset acknowledge error output

          WHEN slv_ack_cmd =>                          --receiving slave acknowledge (command)
            IF to_x01(sda) /= '0' OR ack_error = '1' THEN  --no-acknowledge or previous no-acknowledge
              ack_error <= '1';                     --set error output if no-acknowledge
            END IF;
            
          WHEN rd =>                                --receiving slave data
            data_rx(conv_integer(bit_cnt)) <= to_x01(sda);                --receive current slave data bit
          
          WHEN slv_ack_data =>                          --receiving slave acknowledge (write)
            IF to_x01(sda) /= '0' OR ack_error = '1' THEN  --no-acknowledge or previous no-acknowledge
              ack_error <= '1';                     --set error output if no-acknowledge
            END IF;
          
          WHEN OTHERS =>
            NULL;
          
        END CASE;
      END IF;
    END IF;
  END PROCESS; 
  
  do_sda : process(all) is  
  variable sda_ena_n : std_logic;
  begin       
    case state is
      when start | restart =>
        sda_ena_n := data_clk_prev;
      when wait_restart | contxfer =>
        sda_ena_n := '1';
      when stop =>
        sda_ena_n := not data_clk_prev;
      when others =>
        sda_ena_n := sda_int;
    end case;
    
    if sda_ena_n = '0' then
      sda <= '0';
    else
      sda <= 'Z';
    end if;
  end process;
  
  do_scl : process(all) is 
  variable scl_clk_prev : std_logic;
  begin      
    if reset_n = '0' then
      scl <= '1';
      scl_clk_prev := '0';
    elsif rising_edge(clk) then
      
      if scl_clk /= scl_clk_prev then
        case state is
          when ready =>
            scl <= '1';
          when wait_restart =>
            scl <= '0';
          when contxfer =>
            scl <= '0';
          when stop =>
            scl <= '1';
          when others =>
            scl <= scl_clk;
        end case; 
      end if;

      scl_clk_prev := scl_clk;
    end if;
  end process;


END logic;