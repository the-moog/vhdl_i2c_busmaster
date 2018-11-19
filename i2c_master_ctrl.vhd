-------------------------------------------------------------------------------
--
--  Title      Modular VHDL peripheral - I2C bus master controller
--             https://github.com/the-moog/vhdl_modular_blocks
--  File       i2c_master_ctrl.vhd
--  Author     Jason Morgan
--
--  Copyright  © Jason Morgan 2018
--  License    This work is licensed under a Creative Commons Attribution-NoDerivatives 4.0 International License.
--             CC-BY-ND, see LICENSE.TXT
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

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_lOGIC_ARITH.all;
use work.utils.all;    
use work.types.all;
use work.modules.all;


/*!
@brief   MODULAR PERIPHERAL: An I2C bus master controller register interface
@extends modules.vhd, see https://github.com/the-moog/vhdl_modular_blocks
@extends i2c_master2.vhd, see https://github.com/the-moog/vhdl_i2c_busmaster
@details The actual I2C module is third party. This module is the register interface to it.
         This has three registers, address, data and control<BR>
*/
entity i2c_master_ctrl is 
  GENERIC(
    input_clk : INTEGER := 50_000_000; --input clock speed from user logic in Hz
    bus_clk   : INTEGER := 400_000);   --speed the i2c bus (scl) will run at in Hz
  port (                  
    clk  : in std_logic;
    rst  : in std_logic; 
    module : in module_t;
    addr : in std_logic_vector;
    data : inout std_logic_vector(15 downto 0); --Sampled on the rising edge of clk
    irq  : out std_logic;
    size : out positive;
    sda  : INOUT  STD_LOGIC;                    --serial data output of i2c bus
    scl  : INOUT  STD_LOGIC;                    --serial clock output of i2c bus  
    dbg  : out std_logic;
    cs : in std_logic;      --Module enable active high, sampled on the rising edge of clk
    rd_nwr : in std_logic); --Read/not Write
end entity;


architecture behavior of i2c_master_ctrl is

component i2c_master IS
  GENERIC(
    input_clk : INTEGER := 50_000_000; --input clock speed from user logic in Hz
    bus_clk   : INTEGER := 400_000);   --speed the i2c bus (scl) will run at in Hz
  PORT(
    clk       : IN     STD_LOGIC;                    --system clock
    reset_n   : IN     STD_LOGIC;                    --active low reset 
    nbits     : IN     unsigned(3 downto 0);         --Nbits to transfer less one
    ena       : IN     STD_LOGIC;                    --latch in command  
    continue  : IN     STD_LOGIC;    
    rep_start : in     std_logic;
    addr      : IN     STD_LOGIC_VECTOR(6 DOWNTO 0); --address of target slave
    rw        : IN     STD_LOGIC;                    --'0' is write, '1' is read
    data_wr   : IN     STD_LOGIC_VECTOR(15 DOWNTO 0); --data to write to slave
    busy      : OUT    STD_LOGIC;                    --indicates transaction in progress
    data_rd   : OUT    STD_LOGIC_VECTOR(15 DOWNTO 0); --data read from slave
    ack_error : BUFFER STD_LOGIC;                    --flag if improper acknowledge from slave
    sda       : INOUT  STD_LOGIC;                    --serial data output of i2c bus
    scl       : INOUT  STD_LOGIC;                   --serial clock output of i2c bus
    dbg       : out    STD_LOGIC);
END component;

constant ZZZ : std_logic_vector(data'range) := (others => 'Z');       

signal rd_data : std_logic_vector(data'range);
signal i2c_data_rd_reg : std_logic_vector(15 downto 0);
signal i2c_data_wr_reg : std_logic_vector(15 downto 0);
signal i2c_addr_reg : std_logic_vector(7 downto 0);
alias i2c_dev_addr is i2c_addr_reg(7 downto 1);
alias i2c_rw_bit is i2c_addr_reg(0);  
signal i2c_ctrl_reg : std_logic_vector(7 downto 0);
alias num_bit_reg is i2c_ctrl_reg(7 downto 4);  
alias busy is i2c_ctrl_reg(0);
alias ack_error is i2c_ctrl_reg(1); 
alias continue is i2c_ctrl_reg(2);
alias rep_start is i2c_ctrl_reg(3);

signal ena : std_logic;
signal nbits : unsigned(3 downto 0);  

signal rep_start_i : std_logic;
signal start_flag : std_logic;  

signal clear_irq_flag : std_logic;

constant sizei : positive := 3; --need to use an intermediate constant here to keep synplicity happy
constant addrbits : positive := log2(sizei - 1);    
begin                                                       
  
  --dbg <= busy;

  data <= rd_data when rd_nwr = '1' and cs = '1' else ZZZ;      
  
  size <= sizei;     
  
  nbits <= unsigned(num_bit_reg);
  
  do_read : process (all) is
  variable address : unsigned(addrbits - 1 downto 0);
  begin                                 
    address := unsigned(addr(address'range)) - module.base;
    rd_data <= (others => '0');
    case conv_integer(address) is
      when 0 =>  -- Address register
        rd_data <= (15 downto 8 => '0') & i2c_addr_reg;
      when 1 =>  -- Data register
        if i2c_rw_bit = '1' then
          --if the next or last transaction was a read return the data received
          rd_data <= i2c_data_rd_reg;
        else                                                                   
          --if the last transaction was a write, return the data to be sent
          rd_data <= i2c_data_wr_reg;
        end if;
      when 2 =>  -- Control register
        rd_data <= (15 downto 8 => '0') & i2c_ctrl_reg;
      when others =>
        null;
    end case;
  end process;
  
  do_write : process(clk, rst, addr, cs, rd_nwr, data) is
  variable address : unsigned(addrbits - 1 downto 0);
  begin 
    address := unsigned(addr(address'range)) - module.base;
    if rst = '1' then
      i2c_data_wr_reg <= (others => '0');
      i2c_addr_reg <= (others => '0');
      start_flag <= '0';
      clear_irq_flag <= '0';
      continue <= '0';
      rep_start <= '0';
      num_bit_reg <= "0111";
      rep_start_i <= '0';
    elsif rising_edge(clk) then --Process regs on rising edge
      
      if busy = '1' then
        start_flag <= '0';
      end if;

      clear_irq_flag <= '0';

      if cs = '1' then
        --Reading or writing the control register clears the IRQ
        --Reading the data register clears the IRQ
        if conv_integer(address) = 2 or (rd_nwr = '1' and conv_integer(address) = 1) then
          clear_irq_flag <= '1';
        end if;    
        
        --For reads, reading one byte causes a new transfer if continue or rep_start was set previously
        if rd_nwr = '1' and conv_integer(address) = 1 and i2c_rw_bit = '1' and continue = '1' then
          start_flag <= '1';
        end if;
      end if;
      
      --Handle writes
      if cs = '1' and rd_nwr = '0' then
        case conv_integer(address) is
          when 0 => -- Address register
            if data(0) = '1' then
              --A write to the address register triggers a first read
              start_flag <= '1';
              --Regardless of the state of rep_start a start bit is generated for addresses
              rep_start_i <= '1';
            end if;
            i2c_addr_reg <= data(7 downto 0);  
          when 1 =>  -- Data register
            if i2c_rw_bit = '0' then
              --As this is a write we start transfers on the data write
              start_flag <= '1';
              rep_start_i <= rep_start;
            end if;
            i2c_data_wr_reg <= data;
          when 2 =>  -- Control register
            num_bit_reg <= data(7 downto 4);
            continue <= data(2);
            rep_start <= data(3);
          when others =>
          null; 
        end case;
      end if;
    end if;    
  end process;   
  
  do_ctrl : process(all) is
  --variable address : unsigned(addrbits - 1 downto 0);
  variable oldbusy : std_logic;
  begin   
    --address := unsigned(addr(address'range)) - module.base;
    if rst = '1' then
      --continue_i <= continue;
      ena <= '0';
      irq <= '0';
      oldbusy := '0';
    elsif rising_edge(clk) then
      
      ena <= '0';
      
      if oldbusy = '1' and busy ='0' then
        --Assert IRQ on the falling edge of busy
        irq <= '1';
      end if;
      
      oldbusy := busy;

      if start_flag = '1' and busy = '0' then
        ena <= '1';                     --Trigger a transaction
        irq <= '0';                     --Clear any pending interrupt
      end if;     
      
      if clear_irq_flag = '1' then
        irq <= '0';
      end if;
    end if;
    
  end process;
  
  inst_i2c_master : i2c_master generic map ( input_clk => input_clk, bus_clk => bus_clk )
  port map (                                                   
    clk       => clk,
    reset_n   => not rst,  
    nbits     => nbits,
    ena       => ena,
    continue  => continue,
    rep_start => rep_start_i,
    addr      => i2c_dev_addr,
    rw        => i2c_rw_bit,
    data_wr   => i2c_data_wr_reg,
    busy      => busy,
    data_rd   => i2c_data_rd_reg,
    ack_error => ack_error,
    sda       => sda,
    scl       => scl,
    dbg       => dbg
  );
  
  
end architecture;
    
    
    
    