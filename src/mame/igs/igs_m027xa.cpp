// license:BSD-3-Clause
// copyright-holders: Xing Xing, David Haywood

/*

IGS ARM7 (IGS027A) based Mahjong / Gambling platform(s) with XA sub-cpu
These games use the IGS027A processor.

*/

#include "emu.h"

#include "igs017_igs031.h"
#include "igs027a.h"
#include "pgmcrypt.h"

#include "cpu/arm7/arm7core.h"
#include "cpu/xa/xa.h"

#include "machine/i8255.h"
#include "machine/nvram.h"
#include "machine/ticket.h"
#include "machine/timer.h"

#include "sound/okim6295.h"

#include "screen.h"
#include "speaker.h"

#include "crzybugs.lh"

#define LOG_DEBUG       (1U << 1)
//#define VERBOSE         (LOG_DEBUG)
#include "logmacro.h"

namespace {

class igs_m027xa_state : public driver_device
{
public:
	igs_m027xa_state(const machine_config &mconfig, device_type type, const char *tag) :
		driver_device(mconfig, type, tag),
		m_igs_mainram(*this, "igs_mainram"),
		m_maincpu(*this, "maincpu"),
		m_xa(*this, "xa"),
		m_ppi(*this, "ppi8255"),
		m_igs017_igs031(*this, "igs017_igs031"),
		m_oki(*this, "oki"),
		m_screen(*this, "screen"),
		m_ticket(*this, "ticket"),
		m_external_rom(*this, "user1"),
		m_io_test(*this, "TEST%u", 0U),
		m_io_dsw(*this, "DSW%u", 1U),
		m_out_lamps(*this, "lamp%u", 1U)
	{ }

	void igs_mahjong_xa(machine_config &config);
	void igs_mahjong_xa_xor(machine_config &config);
	void igs_mahjong_xa_xor_disable(machine_config &config);

	void init_crzybugs();
	void init_crzybugsj();
	void init_hauntedh();
	void init_tripfev();
	void init_wldfruit();

protected:
	virtual void machine_start() override;
	virtual void machine_reset() override;
	virtual void video_start() override;

private:
	optional_shared_ptr<u32> m_igs_mainram;
	required_device<igs027a_cpu_device> m_maincpu;
	required_device<mx10exa_cpu_device> m_xa;
	required_device<i8255_device> m_ppi;
	required_device<igs017_igs031_device> m_igs017_igs031;
	required_device<okim6295_device> m_oki;
	required_device<screen_device> m_screen;
	optional_device<ticket_dispenser_device> m_ticket;
	required_region_ptr<u32> m_external_rom;

	optional_ioport_array<3> m_io_test;
	optional_ioport_array<3> m_io_dsw;

	output_finder<8> m_out_lamps;

	u32 m_xor_table[0x100];
	u8 m_io_select[2];

	u8 m_port2_latch;
	u8 m_port0_latch;
	u32 m_xa_cmd;
	u32 m_xa_ret0;
	bool m_irq_from_igs031;
	bool m_irq_from_xa;
	s8 m_num_params;
	u8 m_port0_dat;
	u8 m_port1_dat;
	u8 m_port2_dat;
	u8 m_port3_dat;

	u32 m_igs_40000014;

	TIMER_DEVICE_CALLBACK_MEMBER(interrupt);

	void pgm_create_dummy_internal_arm_region();
	void main_map(address_map &map);
	void main_xor_map(address_map &map);

	u32 external_rom_r(offs_t offset);

	void xor_table_w(offs_t offset, u8 data);

	u16 xa_r(offs_t offset, u16 mem_mask);
	void xa_w(offs_t offset, u16 data, u16 mem_mask);

	void output_w(u8 data);
	void lamps_w(u8 data);

	void igs_40000014_w(offs_t offset, u32 data, u32 mem_mask);

	u8 mcu_p0_r();
	u8 mcu_p1_r();
	u8 mcu_p2_r();
	u8 mcu_p3_r();
	void mcu_p0_w(uint8_t data);
	void mcu_p1_w(uint8_t data);
	void mcu_p2_w(uint8_t data);
	void mcu_p3_w(uint8_t data);

	u32 gpio_r();
	void oki_bank_w(offs_t offset, u8 data);
	template <unsigned Select, unsigned First> u8 dsw_r();
	template <unsigned Select> void io_select_w(u8 data);
};


void igs_m027xa_state::machine_reset()
{
	m_port2_latch = 0;
	m_port0_latch = 0;
	m_xa_cmd = 0;
	m_xa_ret0 = 0;
	m_irq_from_igs031 = false;
	m_irq_from_xa = false;
	m_num_params = 0;
	m_port0_dat = 0;
	m_port1_dat = 0;
	m_port2_dat = 0;
	m_port3_dat = 0;

	m_igs_40000014 = 0;
}

void igs_m027xa_state::machine_start()
{
	m_out_lamps.resolve();

	std::fill(std::begin(m_xor_table), std::end(m_xor_table), 0);

	save_item(NAME(m_xor_table));
	save_item(NAME(m_io_select));

	save_item(NAME(m_port2_latch));
	save_item(NAME(m_port0_latch));
	save_item(NAME(m_xa_cmd));
	save_item(NAME(m_xa_ret0));
	save_item(NAME(m_irq_from_igs031));
	save_item(NAME(m_irq_from_xa));
	save_item(NAME(m_num_params));
	save_item(NAME(m_port0_dat));
	save_item(NAME(m_port1_dat));
	save_item(NAME(m_port2_dat));
	save_item(NAME(m_port3_dat));

	save_item(NAME(m_igs_40000014));
}

void igs_m027xa_state::video_start()
{
	m_igs017_igs031->video_start();
}

/***************************************************************************

    Memory Maps

***************************************************************************/

void igs_m027xa_state::main_map(address_map &map)
{
	map(0x08000000, 0x0807ffff).rom().region("user1", 0); // Game ROM
	map(0x10000000, 0x100003ff).ram().share("igs_mainram"); // main RAM for ASIC?
	map(0x18000000, 0x18007fff).ram();

	map(0x38000000, 0x38007fff).rw(m_igs017_igs031, FUNC(igs017_igs031_device::read), FUNC(igs017_igs031_device::write));
	map(0x38008000, 0x38008003).umask32(0x000000ff).rw(m_oki, FUNC(okim6295_device::read), FUNC(okim6295_device::write));
	map(0x38009000, 0x38009003).rw(m_ppi, FUNC(i8255_device::read), FUNC(i8255_device::write));
	map(0x3800c000, 0x3800c003).w(FUNC(igs_m027xa_state::oki_bank_w));
	map(0x40000014, 0x40000017).w(FUNC(igs_m027xa_state::igs_40000014_w));

	map(0x50000000, 0x500003ff).umask32(0x000000ff).w(FUNC(igs_m027xa_state::xor_table_w));

	map(0x58000000, 0x580000ff).rw(FUNC(igs_m027xa_state::xa_r), FUNC(igs_m027xa_state::xa_w));
}

void igs_m027xa_state::main_xor_map(address_map &map)
{
	main_map(map);

	map(0x08000000, 0x0807ffff).r(FUNC(igs_m027xa_state::external_rom_r)); // Game ROM
}

static INPUT_PORTS_START( base )
	PORT_START("TEST0")
	PORT_BIT( 0x01, IP_ACTIVE_LOW, IPT_UNKNOWN )
	PORT_BIT( 0x02, IP_ACTIVE_LOW, IPT_GAMBLE_BET )     PORT_NAME("Play")
	PORT_SERVICE_NO_TOGGLE( 0x04, IP_ACTIVE_LOW )
	PORT_BIT( 0x08, IP_ACTIVE_LOW, IPT_GAMBLE_BOOK)
	PORT_BIT( 0x10, IP_ACTIVE_LOW, IPT_COIN1 )          // COINA
	PORT_BIT( 0x20, IP_ACTIVE_LOW, IPT_UNKNOWN )
	PORT_BIT( 0x40, IP_ACTIVE_LOW, IPT_UNKNOWN )
	PORT_BIT( 0x80, IP_ACTIVE_LOW, IPT_GAMBLE_HIGH )    PORT_NAME("Big")

	PORT_START("TEST1")
	PORT_BIT( 0x01, IP_ACTIVE_LOW, IPT_GAMBLE_KEYIN )
	PORT_BIT( 0x02, IP_ACTIVE_LOW, IPT_GAMBLE_KEYOUT )
	PORT_BIT( 0x04, IP_ACTIVE_LOW, IPT_START1 )         PORT_NAME("Start / Stop All Reels")
	PORT_BIT( 0x08, IP_ACTIVE_LOW, IPT_BUTTON2 )        PORT_NAME("Ticket")
	PORT_BIT( 0x10, IP_ACTIVE_LOW, IPT_CUSTOM )         PORT_READ_LINE_DEVICE_MEMBER("ticket", ticket_dispenser_device, line_r)
	PORT_BIT( 0x20, IP_ACTIVE_LOW, IPT_UNKNOWN )
	PORT_BIT( 0x40, IP_ACTIVE_LOW, IPT_UNKNOWN )
	PORT_BIT( 0x80, IP_ACTIVE_LOW, IPT_COIN2 )          // COINC

	PORT_START("TEST2")
	PORT_BIT( 0x00007, IP_ACTIVE_LOW, IPT_UNUSED )
	PORT_BIT( 0x00008, IP_ACTIVE_LOW, IPT_SLOT_STOP2 ) PORT_NAME("Stop Reel 2 / Small")
	PORT_BIT( 0x00010, IP_ACTIVE_LOW, IPT_SLOT_STOP3 ) PORT_NAME("Stop Reel 3 / Take Score")
	PORT_BIT( 0x00020, IP_ACTIVE_LOW, IPT_SLOT_STOP1 ) PORT_NAME("Stop Reel 1 / Double Up")
	PORT_BIT( 0xfffc0, IP_ACTIVE_LOW, IPT_UNUSED ) // peripheral interrupts in bits 8 and 9 - see gpio_r

	PORT_START("DSW1")
	PORT_DIPNAME( 0x01, 0x01, DEF_STR(Demo_Sounds) )       PORT_DIPLOCATION("SW1:1")
	PORT_DIPSETTING(    0x00, DEF_STR(Off) )
	PORT_DIPSETTING(    0x01, DEF_STR(On) )
	PORT_DIPNAME( 0x02, 0x02, "Non Stop" )                 PORT_DIPLOCATION("SW1:2")
	PORT_DIPSETTING(    0x00, DEF_STR(Yes) )
	PORT_DIPSETTING(    0x02, DEF_STR(No) )
	PORT_DIPNAME( 0x04, 0x04, "Password" )                 PORT_DIPLOCATION("SW1:3")
	PORT_DIPSETTING(    0x00, DEF_STR(No) )
	PORT_DIPSETTING(    0x04, DEF_STR(Yes) )
	PORT_DIPNAME( 0x08, 0x08, "Odds Table" )               PORT_DIPLOCATION("SW1:4")
	PORT_DIPSETTING(    0x00, DEF_STR(No) )
	PORT_DIPSETTING(    0x08, DEF_STR(Yes) )
	PORT_DIPNAME( 0x10, 0x10, "Double Up Game" )           PORT_DIPLOCATION("SW1:5")
	PORT_DIPSETTING(    0x00, DEF_STR(Off) )
	PORT_DIPSETTING(    0x10, DEF_STR(On) )
	PORT_DIPNAME( 0x60, 0x60, "Symbol" )                   PORT_DIPLOCATION("SW1:6,7")
	PORT_DIPSETTING(    0x00, "Both" )
	PORT_DIPSETTING(    0x20, "Both (duplicate)" )
	PORT_DIPSETTING(    0x40, "Fruit" )
	PORT_DIPSETTING(    0x60, "Bug" )
	PORT_DIPUNKNOWN_DIPLOC( 0x80, 0x80, "SW1:8" )

	PORT_START("DSW2")
	PORT_DIPNAME( 0x03, 0x03, "Score Box" )                PORT_DIPLOCATION("SW2:1,2")
	PORT_DIPSETTING(    0x00, "10X" )
	PORT_DIPSETTING(    0x01, "10X (duplicate)" )
	PORT_DIPSETTING(    0x02, DEF_STR(Yes) )
	PORT_DIPSETTING(    0x03, DEF_STR(No) )
	PORT_DIPNAME( 0x04, 0x04, "Play Score" )               PORT_DIPLOCATION("SW2:3")
	PORT_DIPSETTING(    0x00, DEF_STR(Yes) )
	PORT_DIPSETTING(    0x04, DEF_STR(No) )
	PORT_DIPNAME( 0x08, 0x08, "Hand Count" )               PORT_DIPLOCATION("SW2:4")
	PORT_DIPSETTING(    0x00, DEF_STR(Yes) )
	PORT_DIPSETTING(    0x08, DEF_STR(No) )
	PORT_DIPNAME( 0x30, 0x30, "Hold Pair" )                PORT_DIPLOCATION("SW2:5,6")
	PORT_DIPSETTING(    0x00, "Georgia" )
	PORT_DIPSETTING(    0x10, "Georgia (duplicate)" )
	PORT_DIPSETTING(    0x20, "Regular" )
	PORT_DIPSETTING(    0x30, DEF_STR(Off) )
	PORT_DIPNAME( 0x40, 0x40, "Auto Hold" )                PORT_DIPLOCATION("SW2:7")
	PORT_DIPSETTING(    0x00, DEF_STR(Yes) )
	PORT_DIPSETTING(    0x40, DEF_STR(No) )
	PORT_DIPUNKNOWN_DIPLOC( 0x80, 0x80, "SW2:8" )

	PORT_START("DSW3")
	PORT_DIPUNKNOWN_DIPLOC( 0x01, 0x01, "SW3:1" )
	PORT_DIPUNKNOWN_DIPLOC( 0x02, 0x02, "SW3:2" )
	PORT_DIPUNKNOWN_DIPLOC( 0x04, 0x04, "SW3:3" )
	PORT_DIPUNKNOWN_DIPLOC( 0x08, 0x08, "SW3:4" )
	PORT_DIPUNKNOWN_DIPLOC( 0x10, 0x10, "SW3:5" )
	PORT_DIPUNKNOWN_DIPLOC( 0x20, 0x20, "SW3:6" )
	PORT_DIPUNKNOWN_DIPLOC( 0x40, 0x40, "SW3:7" )
	PORT_DIPUNKNOWN_DIPLOC( 0x80, 0x80, "SW3:8" )
INPUT_PORTS_END


u16 igs_m027xa_state::xa_r(offs_t offset, u16 mem_mask)
{
	u32 data = ~u32(0);

	switch (offset * 2)
	{
	case 0:
		data = m_xa_ret0;
		break;
	}
	return data;
}


void igs_m027xa_state::output_w(u8 data)
{
	machine().bookkeeping().coin_counter_w(0, BIT(data, 0)); // one pulse per COINA accepted
	machine().bookkeeping().coin_counter_w(2, BIT(data, 1)); // one pulse per key-in accepted
	machine().bookkeeping().coin_counter_w(1, BIT(data, 2)); // one pulse per COINC accepted
	// bits 3 and 4 have something to do with counting credits paid out by ticket and key-out
	if (m_ticket)
		m_ticket->motor_w(BIT(data, 6));
}

void igs_m027xa_state::lamps_w(u8 data)
{
	// active high outputs
	// +------+----------------+
	// | lamp | crzybugs       |
	// +------+----------------+
	// |  1   | stop all/start |
	// |  2   | stop 2/small   |
	// |  3   | bet            |
	// |  4   | stop 3/take    |
	// |  5   | stop 1/double  |
	// |  6   | big            |
	// |  7   |                |
	// |  8   |                |
	// +------+----------------+
	for (unsigned i = 0; 8 > i; ++i)
		m_out_lamps[i] = BIT(data, i);
}


void igs_m027xa_state::igs_40000014_w(offs_t offset, u32 data, u32 mem_mask)
{
	// sets bit 1 before waiting on FIRQ, maybe it's an enable here?
	m_igs_40000014 = data;
}

u32 igs_m027xa_state::gpio_r()
{
	u32 ret = m_io_test[2].read_safe(0xfffff);
	if (m_irq_from_igs031)
		ret &= ~(u32(1) << 8);
	if (m_irq_from_xa)
		ret &= ~(u32(1) << 9);
	return ret;
}

void igs_m027xa_state::oki_bank_w(offs_t offset, u8 data)
{
	if (offset == 0)
		m_oki->set_rom_bank(data & 3);
}

template <unsigned Select, unsigned First>
u8 igs_m027xa_state::dsw_r()
{
	u8 data = 0xff;

	for (int i = First; i < m_io_dsw.size(); i++)
		if (!BIT(m_io_select[Select], i - First))
			data &= m_io_dsw[i].read_safe(0xff);
	return data;
}

template <unsigned Select>
void igs_m027xa_state::io_select_w(u8 data)
{
	m_io_select[Select] = data;
}

void igs_m027xa_state::xa_w(offs_t offset, u16 data, u16 mem_mask)
{
	m_xa_cmd = data;

	if (offset == 0)
	{
		m_num_params--;

		if (m_num_params <= 0)
		{
			LOGMASKED(LOG_DEBUG, "---------------m_xa_cmd is %02x size %02x\n", (data & 0xff00)>>8, data & 0xff);
			m_num_params = data & 0xff;
		}
		else
		{
			LOGMASKED(LOG_DEBUG, "-------------------------- param %04x\n", data & 0xffff);
		}
		m_xa->set_input_line(XA_EXT_IRQ0, ASSERT_LINE);
	}
	else
	{
		m_irq_from_xa = false;
		LOGMASKED(LOG_DEBUG, "%s: unhandled xa_w %04x %08x (%08x)\n", machine().describe_context(), offset * 2, data, mem_mask);
	}
}


u8 igs_m027xa_state::mcu_p0_r()
{
	u8 ret = m_port0_latch;
	LOGMASKED(LOG_DEBUG, "%s: COMMAND READ LOWER mcu_p0_r() returning %02x with port3 as %02x\n", machine().describe_context(), ret, m_port3_dat);
	return ret;
}

u8 igs_m027xa_state::mcu_p1_r()
{
	LOGMASKED(LOG_DEBUG, "%s: mcu_p1_r()\n", machine().describe_context());
	return m_port1_dat;
}

u8 igs_m027xa_state::mcu_p2_r()
{
	u8 ret = m_port2_latch;
	LOGMASKED(LOG_DEBUG, "%s: COMMAND READ mcu_p2_r() returning %02x with port3 as %02x\n", machine().describe_context(), ret, m_port3_dat);
	return m_port2_latch;
}

u8 igs_m027xa_state::mcu_p3_r()
{
	LOGMASKED(LOG_DEBUG, "%s: mcu_p3_r()\n", machine().describe_context());
	return m_port3_dat;
}

template <typename T>
constexpr bool posedge(T oldval, T val, unsigned bit)
{
	return BIT(~oldval & val, bit);
}

template <typename T>
constexpr bool negedge(T oldval, T val, unsigned bit)
{
	return BIT(oldval & ~val, bit);
}

void igs_m027xa_state::mcu_p0_w(uint8_t data)
{
	LOGMASKED(LOG_DEBUG, "%s: mcu_p0_w() %02x with port 3 as %02x and port 1 as %02x\n", machine().describe_context(), data, m_port3_dat, m_port1_dat);
	m_port0_dat = data;
}

void igs_m027xa_state::mcu_p1_w(uint8_t data)
{
	LOGMASKED(LOG_DEBUG, "%s: mcu_p1_w() %02x\n", machine().describe_context(), data);
	m_port1_dat = data;
}

void igs_m027xa_state::mcu_p2_w(uint8_t data)
{
	m_port2_dat = data;
	LOGMASKED(LOG_DEBUG, "%s: mcu_p2_w() %02x with port 3 as %02x\n", machine().describe_context(), data, m_port3_dat);
}

void igs_m027xa_state::mcu_p3_w(uint8_t data)
{
	u8 oldport3 = m_port3_dat;
	m_port3_dat = data;
	LOGMASKED(LOG_DEBUG, "%s: mcu_p3_w() %02x - do latches oldport3 %02x newport3 %02x\n", machine().describe_context(), data, oldport3, m_port3_dat);

	if (posedge(oldport3, m_port3_dat, 5))
	{
		m_irq_from_xa = true;
		m_maincpu->trigger_irq(3);
	}
	// high->low transition on bit 0x80 must read into latches!
	if (negedge(oldport3, m_port3_dat, 7))
	{
		LOGMASKED(LOG_DEBUG, "read command [%d] = [%04x]\n", m_port1_dat & 7, m_xa_cmd);
		m_port2_latch = (m_xa_cmd & 0xff00) >> 8;
		m_port0_latch = m_xa_cmd & 0x00ff;
	}

	if (negedge(oldport3, m_port3_dat, 6))
	{
		uint32_t dat = (m_port2_dat << 8) | m_port0_dat;
		LOGMASKED(LOG_DEBUG, "write command [%d] = [%04x]\n", m_port1_dat & 7, dat);
		m_xa_ret0 = dat;
	}
}

u32 igs_m027xa_state::external_rom_r(offs_t offset)
{
	return m_external_rom[offset] ^ m_xor_table[offset & 0x00ff];
}


void igs_m027xa_state::xor_table_w(offs_t offset, u8 data)
{
	m_xor_table[offset] = (u32(data) << 24) | (u32(data) << 8);
}



TIMER_DEVICE_CALLBACK_MEMBER(igs_m027xa_state::interrupt)
{
	int scanline = param;

	// should be using m_maincpu->trigger_irq with more compelx interrupt logic?

	if (scanline == 240 && m_igs017_igs031->get_irq_enable())
	{
		m_irq_from_igs031 = true;
		m_maincpu->trigger_irq(3);
	}
	if (scanline == 0 && (m_igs_40000014 & 1))
		m_maincpu->pulse_input_line(ARM7_FIRQ_LINE, m_maincpu->minimum_quantum_time()); // vbl?
}


void igs_m027xa_state::igs_mahjong_xa(machine_config &config)
{
	IGS027A(config, m_maincpu, 22'000'000); // Crazy Bugs has a 22MHz crystal, what about the others?
	m_maincpu->set_addrmap(AS_PROGRAM, &igs_m027xa_state::main_map);
	m_maincpu->in_port().set(FUNC(igs_m027xa_state::gpio_r));
	m_maincpu->out_port().set(FUNC(igs_m027xa_state::io_select_w<1>));

//  NVRAM(config, "nvram", nvram_device::DEFAULT_ALL_0);

	MX10EXA(config, m_xa, 10'000'000); // MX10EXAQC (Philips 80C51 XA) unknown frequency
	m_xa->port_in_cb<0>().set(FUNC(igs_m027xa_state::mcu_p0_r));
	m_xa->port_in_cb<1>().set(FUNC(igs_m027xa_state::mcu_p1_r));
	m_xa->port_in_cb<2>().set(FUNC(igs_m027xa_state::mcu_p2_r));
	m_xa->port_in_cb<3>().set(FUNC(igs_m027xa_state::mcu_p3_r));
	m_xa->port_out_cb<0>().set(FUNC(igs_m027xa_state::mcu_p0_w));
	m_xa->port_out_cb<1>().set(FUNC(igs_m027xa_state::mcu_p1_w));
	m_xa->port_out_cb<2>().set(FUNC(igs_m027xa_state::mcu_p2_w));
	m_xa->port_out_cb<3>().set(FUNC(igs_m027xa_state::mcu_p3_w));

	SCREEN(config, m_screen, SCREEN_TYPE_RASTER);
	m_screen->set_refresh_hz(60);
	m_screen->set_vblank_time(ATTOSECONDS_IN_USEC(0));
	m_screen->set_size(512, 256);
	m_screen->set_visarea(0, 512-1, 0, 240-1);
	m_screen->set_screen_update("igs017_igs031", FUNC(igs017_igs031_device::screen_update));
	m_screen->set_palette("igs017_igs031:palette");

	TIMER(config, "scantimer").configure_scanline(FUNC(igs_m027xa_state::interrupt), "screen", 0, 1);

	// crzybugs: PPI port A = input, port B = output, port C = output
	I8255A(config, m_ppi);
	m_ppi->tri_pa_callback().set_constant(0x00);
	m_ppi->tri_pb_callback().set_constant(0x00);
	m_ppi->tri_pc_callback().set_constant(0x00);
	m_ppi->out_pb_callback().set(FUNC(igs_m027xa_state::output_w));
	m_ppi->out_pc_callback().set(FUNC(igs_m027xa_state::lamps_w));

	IGS017_IGS031(config, m_igs017_igs031, 0);
	m_igs017_igs031->set_text_reverse_bits(true);
	m_igs017_igs031->in_pa_callback().set(NAME((&igs_m027xa_state::dsw_r<1, 0>)));
	m_igs017_igs031->in_pb_callback().set_ioport("TEST0");
	m_igs017_igs031->in_pc_callback().set_ioport("TEST1");

	TICKET_DISPENSER(config, m_ticket, attotime::from_msec(200));

	// sound hardware
	SPEAKER(config, "mono").front_center();
	OKIM6295(config, m_oki, 1000000, okim6295_device::PIN7_HIGH).add_route(ALL_OUTPUTS, "mono", 0.5);
}

void igs_m027xa_state::igs_mahjong_xa_xor(machine_config &config)
{
	igs_mahjong_xa(config);

	m_maincpu->set_addrmap(AS_PROGRAM, &igs_m027xa_state::main_xor_map);
}

void igs_m027xa_state::igs_mahjong_xa_xor_disable(machine_config &config)
{
	igs_mahjong_xa_xor(config);

	m_xa->set_disable();
}



// prg at u34
// text at u15
// cg at u32 / u12
// samples at u3

ROM_START( haunthig )
	ROM_REGION( 0x04000, "maincpu", 0 )
	// Internal ROM of IGS027A ARM based MCU
	ROM_LOAD( "haunthig_igs027a", 0x00000, 0x4000, NO_DUMP )

	ROM_REGION32_LE( 0x80000, "user1", 0 ) // external ARM data / prg
	ROM_LOAD( "hauntedhouse_ver-109us.u34", 0x000000, 0x80000, CRC(300fed78) SHA1(afa4c8855cd780c57d4f92ea6131ed4e77063268) )

	ROM_REGION( 0x10000, "xa", 0 )
	ROM_LOAD( "hauntedhouse.u17", 0x000000, 0x10000, BAD_DUMP CRC(3c76b157) SHA1(d8d3a434fd649577a30d5855e3fb34998041f4e5) ) // not dumped for this set

	ROM_REGION( 0x80000, "igs017_igs031:tilemaps", 0 )
	ROM_LOAD16_WORD_SWAP( "haunted-h_text.u15", 0x000000, 0x80000, CRC(c23f48c8) SHA1(0cb1b6c61611a081ae4a3c0be51812045ff632fe) )

	// are these PGM-like sprites?
	ROM_REGION( 0x800000, "igs017_igs031:sprites", 0 )
	ROM_LOAD( "haunted-h_cg.u32",  0x000000, 0x400000, BAD_DUMP CRC(e0ea10e6) SHA1(e81be78fea93e72d4b1f4c0b58560bda46cf7948) ) // not dumped for this set, FIXED BITS (xxxxxxx0xxxxxxxx)
	ROM_LOAD( "haunted-h_ext.u12", 0x400000, 0x400000, BAD_DUMP CRC(662eb883) SHA1(831ebe29e1e7a8b2c2fff7fbc608975771c3486c) ) // not dumped for this set, FIXED BITS (xxxxxxxx0xxxxxxx)

	ROM_REGION( 0x200000, "oki", 0 ) // Oki M6295 samples, missing sample table, bad?
	ROM_LOAD( "haunted-h_sp.u3", 0x00000, 0x200000,  BAD_DUMP CRC(fe3fcddf) SHA1(ac57ab6d4e4883747c093bd19d0025cf6588cb2c) ) // not dumped for this set

	ROM_REGION( 0x500, "plds", ROMREGION_ERASE00 )
	ROM_LOAD( "hu_u38a.u38", 0x000, 0x117, NO_DUMP ) // ATF16V8B, protected
	ROM_LOAD( "hu_u39.u39",  0x200, 0x2dd, CRC(75f58b46) SHA1(7cb136a41899ddd50c95a67ca6353ce5d8d92149) ) // AT22V10
ROM_END

ROM_START( haunthiga ) // IGS PCB-0575-04-HU - Has IGS027A, MX10EXAQC, IGS031, Oki M6295, 2x 8-dip banks
	ROM_REGION( 0x04000, "maincpu", 0 )
	// Internal ROM of IGS027A ARM based MCU
	ROM_LOAD( "h2_igs027a", 0x00000, 0x4000, NO_DUMP ) // sticker marked 'H2'

	ROM_REGION32_LE( 0x80000, "user1", 0 ) // external ARM data / prg
	ROM_LOAD( "hauntedhouse_ver-101us.u34", 0x000000, 0x80000, CRC(4bf045d4) SHA1(78c848fd69961df8d9b75f92ad57c3534fbf08db) )

	ROM_REGION( 0x10000, "xa", 0 )
	ROM_LOAD( "hauntedhouse.u17", 0x000000, 0x10000, CRC(3c76b157) SHA1(d8d3a434fd649577a30d5855e3fb34998041f4e5) ) // MX10EXAQC (80C51 XA based MCU) marked J9, not read protected?

	ROM_REGION( 0x80000, "igs017_igs031:tilemaps", 0 )
	ROM_LOAD16_WORD_SWAP( "haunted-h_text.u15", 0x000000, 0x80000, CRC(c23f48c8) SHA1(0cb1b6c61611a081ae4a3c0be51812045ff632fe) )

	// are these PGM-like sprites?
	ROM_REGION( 0x800000, "igs017_igs031:sprites", 0 )
	ROM_LOAD( "haunted-h_cg.u32",  0x000000, 0x400000, CRC(e0ea10e6) SHA1(e81be78fea93e72d4b1f4c0b58560bda46cf7948) ) // FIXED BITS (xxxxxxx0xxxxxxxx)
	ROM_LOAD( "haunted-h_ext.u12", 0x400000, 0x400000, CRC(662eb883) SHA1(831ebe29e1e7a8b2c2fff7fbc608975771c3486c) ) // FIXED BITS (xxxxxxxx0xxxxxxx)

	ROM_REGION( 0x200000, "oki", 0 ) // Oki M6295 samples, missing sample table, bad?
	ROM_LOAD( "haunted-h_sp.u3", 0x00000, 0x200000, BAD_DUMP CRC(fe3fcddf) SHA1(ac57ab6d4e4883747c093bd19d0025cf6588cb2c) )

	ROM_REGION( 0x500, "plds", ROMREGION_ERASE00 )
	ROM_LOAD( "hu_u38a.u38", 0x000, 0x117, NO_DUMP ) // ATF16V8B, protected
	ROM_LOAD( "hu_u39.u39",  0x200, 0x2dd, CRC(75f58b46) SHA1(7cb136a41899ddd50c95a67ca6353ce5d8d92149) ) // AT22V10
ROM_END

ROM_START( crzybugs ) // IGS PCB-0447-05-GM - Has IGS027A, MX10EXAQC, IGS031, Oki M6295, 3x 8-DIP banks
	ROM_REGION( 0x04000, "maincpu", 0 )
	// Internal ROM of IGS027A ARM based MCU
	ROM_LOAD( "m7_igs27a.u37", 0x00000, 0x4000, CRC(1b20532c) SHA1(e08d0110a843915a8ba8627ae6d3947cccc22048) ) // sticker marked 'M7'

	ROM_REGION32_LE( 0x80000, "user1", 0 ) // external ARM data / prg
	ROM_LOAD( "crazy_bugs_v-204us.u23", 0x000000, 0x80000, CRC(d1232462) SHA1(685a292f39bf57a80d6ef31289cf9f673ba06dd4) ) // MX27C4096

	ROM_REGION( 0x10000, "xa", 0 ) // MX10EXAQC (80C51 XA based MCU) marked J9
	ROM_LOAD( "j9.u27", 0x00000, 0x10000, CRC(3c76b157) SHA1(d8d3a434fd649577a30d5855e3fb34998041f4e5) )

	ROM_REGION( 0x80000, "igs017_igs031:tilemaps", 0 )
	ROM_LOAD16_WORD_SWAP( "crazy_bugs_text_u10.u10", 0x000000, 0x80000, CRC(db0d679a) SHA1(c5d039aa4fa2218b6f574ccb5b6da983b8d4067d) )

	// are these PGM-like sprites?
	ROM_REGION( 0x200000, "igs017_igs031:sprites", 0 )
	ROM_LOAD( "crazy_bugs_cg.u19",  0x000000, 0x200000, CRC(9d53ad47) SHA1(46690a37acf8bd88c7fbe973db2faf5ef0cff805) ) // FIXED BITS (xxxxxxx0xxxxxxxx)
	// u18 not populated

	ROM_REGION( 0x200000, "oki", 0 ) // plain Oki M6295 samples
	ROM_LOAD( "crazybugs_sp.u15", 0x000000, 0x200000, CRC(591b315b) SHA1(fda1816d83e202170dba4afc6e7898b706a76087) ) // M27C160
ROM_END

ROM_START( crzybugsa )
	ROM_REGION( 0x04000, "maincpu", 0 )
	// Internal ROM of IGS027A ARM based MCU
	ROM_LOAD( "m7_igs27a.u37", 0x00000, 0x4000, CRC(1b20532c) SHA1(e08d0110a843915a8ba8627ae6d3947cccc22048) ) // sticker marked 'M7' (not verified for this set)

	ROM_REGION32_LE( 0x80000, "user1", 0 ) // external ARM data / prg
	ROM_LOAD( "crazy_bugs_v-202us.u23", 0x000000, 0x80000, CRC(210da1e6) SHA1(c726497bebd25d6a9053e331b4c26acc7e2db0b2) ) // MX27C4096

	ROM_REGION( 0x10000, "xa", 0 ) // MX10EXAQC (80C51 XA based MCU)
	ROM_LOAD( "j9.u27", 0x00000, 0x10000, CRC(3c76b157) SHA1(d8d3a434fd649577a30d5855e3fb34998041f4e5) )

	ROM_REGION( 0x80000, "igs017_igs031:tilemaps", 0 )
	ROM_LOAD16_WORD_SWAP( "crazy_bugs_text_u10.u10", 0x000000, 0x80000, CRC(db0d679a) SHA1(c5d039aa4fa2218b6f574ccb5b6da983b8d4067d) ) // M27C4002

	// are these PGM-like sprites?
	ROM_REGION( 0x200000, "igs017_igs031:sprites", 0 )
	ROM_LOAD( "crazy_bugs_cg.u19",  0x000000, 0x200000, CRC(9d53ad47) SHA1(46690a37acf8bd88c7fbe973db2faf5ef0cff805) ) // M27C160, FIXED BITS (xxxxxxx0xxxxxxxx)
	// u18 not populated

	ROM_REGION( 0x200000, "oki", 0 ) // plain Oki M6295 samples
	ROM_LOAD( "crazy_bugs_sp.u15", 0x000000, 0x200000, CRC(591b315b) SHA1(fda1816d83e202170dba4afc6e7898b706a76087) ) // M27C160
ROM_END

ROM_START( crzybugsb )
	ROM_REGION( 0x04000, "maincpu", 0 )
	// Internal ROM of IGS027A ARM based MCU
	ROM_LOAD( "m7_igs27a.u37", 0x00000, 0x4000, CRC(1b20532c) SHA1(e08d0110a843915a8ba8627ae6d3947cccc22048) ) // sticker marked 'M7' (not verified for this set)

	ROM_REGION32_LE( 0x80000, "user1", 0 ) // external ARM data / prg
	ROM_LOAD( "crazy_bugs_v-202us.u23", 0x000000, 0x80000, CRC(129e36e9) SHA1(53f20bc3792249de8ef276f84283baa9abd30acd) ) // MX27C4096

	ROM_REGION( 0x10000, "xa", 0 ) // MX10EXAQC (80C51 XA based MCU)
	ROM_LOAD( "j9.u27", 0x00000, 0x10000, CRC(3c76b157) SHA1(d8d3a434fd649577a30d5855e3fb34998041f4e5) )

	ROM_REGION( 0x80000, "igs017_igs031:tilemaps", 0 )
	ROM_LOAD16_WORD_SWAP( "crazy_bugs_text_u10.u10", 0x000000, 0x80000, BAD_DUMP CRC(db0d679a) SHA1(c5d039aa4fa2218b6f574ccb5b6da983b8d4067d) ) // not dumped for this set

	// are these PGM-like sprites?
	ROM_REGION( 0x200000, "igs017_igs031:sprites", 0 )
	ROM_LOAD( "crazy_bugs_cg.u19",  0x000000, 0x200000, BAD_DUMP CRC(9d53ad47) SHA1(46690a37acf8bd88c7fbe973db2faf5ef0cff805) ) // not dumped for this set, FIXED BITS (xxxxxxx0xxxxxxxx)
	// u18 not populated

	ROM_REGION( 0x200000, "oki", 0 ) // plain Oki M6295 samples
	ROM_LOAD( "crazy_bugs_sp.u15", 0x000000, 0x200000, BAD_DUMP CRC(591b315b) SHA1(fda1816d83e202170dba4afc6e7898b706a76087) ) // not dumped for this set
ROM_END

ROM_START( crzybugsj ) // IGS PCB-0575-04-HU - Has IGS027A, MX10EXAQC, IGS031, Oki M6295, 2x 8-dip banks
	ROM_REGION( 0x04000, "maincpu", 0 )
	// Internal ROM of IGS027A ARM based MCU
	ROM_LOAD( "m6.u42", 0x00000, 0x4000, NO_DUMP ) // sticker marked 'M6'

	ROM_REGION32_LE( 0x200000, "user1", 0 ) // external ARM data / prg
	ROM_LOAD( "crazy_bugs_v-103jp.u34", 0x000000, 0x200000, CRC(1e35ed79) SHA1(0e4f8b706cdfcaf2aacdc40eec422df9d865b311) )

	ROM_REGION( 0x10000, "xa", 0 )
	ROM_LOAD( "e9.u17", 0x00000, 0x10000, CRC(3c76b157) SHA1(d8d3a434fd649577a30d5855e3fb34998041f4e5) ) // MX10EXAQC (80C51 XA based MCU) marked E9, same as haunthig

	ROM_REGION( 0x80000, "igs017_igs031:tilemaps", 0 )
	ROM_LOAD16_WORD_SWAP( "crazy_bugs_text_u15.u15", 0x000000, 0x80000, CRC(db0d679a) SHA1(c5d039aa4fa2218b6f574ccb5b6da983b8d4067d) )
	// u14 not populated

	// are these PGM-like sprites?
	ROM_REGION( 0x200000, "igs017_igs031:sprites", 0 )
	ROM_LOAD( "crazy_bugs_ani-cg-u32.u32",  0x000000, 0x200000, CRC(9d53ad47) SHA1(46690a37acf8bd88c7fbe973db2faf5ef0cff805) ) // FIXED BITS (xxxxxxx0xxxxxxxx)
	// u12 not populated

	ROM_REGION( 0x200000, "oki", 0 ) // plain Oki M6295 samples
	ROM_LOAD( "crazy_bugs_sp_u3.u3", 0x000000, 0x200000,  CRC(b15974a1) SHA1(82509902bbb33a2120d815e7879b9b8591a29976) )

	ROM_REGION( 0x500, "plds", ROMREGION_ERASE00 )
	ROM_LOAD( "hu_u38.u38", 0x000, 0x117, NO_DUMP ) // ATF16V8B, protected
	ROM_LOAD( "hu_u39.u39", 0x200, 0x2dd, CRC(75f58b46) SHA1(7cb136a41899ddd50c95a67ca6353ce5d8d92149) ) // AT22V10
ROM_END

ROM_START( tripfev ) // IGS PCB-0447-05-GM - Has IGS027A, MX10EXAQC, IGS031, Oki M6295, 3x 8-DIP banks
	ROM_REGION( 0x04000, "maincpu", 0 )
	// Internal ROM of IGS027A ARM based MCU
	ROM_LOAD( "m1_igs027a.u37", 0x00000, 0x4000, CRC(a40ec1f8) SHA1(f6f7005d61522934758fd0a98bf383c6076b6afe) ) // sticker marked 'M1'

	ROM_REGION32_LE( 0x80000, "user1", 0 ) // external ARM data / prg
	ROM_LOAD( "triple_fever_u23_v107_us.u23", 0x000000, 0x80000, CRC(aa56d888) SHA1(0b8b2765079259b76ea803289841d867c33c8cb2) ) // 27C4096

	ROM_REGION( 0x10000, "xa", 0 ) // MX10EXAQC (80C51 XA based MCU) marked P7
	ROM_LOAD( "p7.u27", 0x00000, 0x10000, NO_DUMP )

	ROM_REGION( 0x80000, "igs017_igs031:tilemaps", 0 )
	ROM_LOAD16_WORD_SWAP( "triple_fever_u10_text.u10", 0x000000, 0x80000, CRC(522a1030) SHA1(9a7a5ba9b26bceb0d251be6139c10e4655fc19ec) ) // M27C4002

	ROM_REGION( 0x400000, "igs017_igs031:sprites", 0 )
	ROM_LOAD( "triple_fever_u19_cg.u19",  0x000000, 0x400000, CRC(cd45bbf2) SHA1(7f1cf270245bbe4604de2cacade279ab13584dbd) ) // M27C322, FIXED BITS (xxxxxxx0xxxxxxxx)
	// u18 not populated

	ROM_REGION( 0x200000, "oki", 0 ) // plain Oki M6295 samples
	ROM_LOAD( "triplef_sp_u15.u15", 0x000000, 0x200000, CRC(98b9cafd) SHA1(3bf3971f0d9520c98fc6b1c2e77ab9c178d21c62) ) // M27C160
ROM_END

ROM_START( wldfruit ) // IGS PCB-0447-05-GM - Has IGS027A, MX10EXAQC, IGS031, Oki M6295, 3x 8-DIP banks
	ROM_REGION( 0x04000, "maincpu", 0 )
	// Internal ROM of IGS027A ARM based MCU
	ROM_LOAD( "w1.u37", 0x00000, 0x4000, NO_DUMP ) // sticker marked 'W1'

	ROM_REGION32_LE( 0x80000, "user1", 0 ) // external ARM data / prg
	ROM_LOAD( "wild_fruit_v-208us.u23", 0x000000, 0x80000, CRC(d43398f1) SHA1(ecc4bd5cb6da16b35c63b843cf7beec1ab84ed9d) ) // M27C4002

	ROM_REGION( 0x10000, "xa", 0 ) // MX10EXAQC (80C51 XA based MCU) marked J9
	ROM_LOAD( "j9.u27", 0x00000, 0x10000, CRC(3c76b157) SHA1(d8d3a434fd649577a30d5855e3fb34998041f4e5) )

	ROM_REGION( 0x80000, "igs017_igs031:tilemaps", 0 )
	ROM_LOAD16_WORD_SWAP( "wild_fruit_text.u10", 0x000000, 0x80000, CRC(d6f0fd58) SHA1(5ddae5d4df53504dbb2e0fe9f7caea961c961ef8) ) // 27C4096

	ROM_REGION( 0x400000, "igs017_igs031:sprites", 0 )
	ROM_LOAD( "wild_fruit_cg.u19",  0x000000, 0x400000, CRC(119686a8) SHA1(22583c1a1018cfdd20f0ef696d91fa1f6e01ab00) ) // M27C322, FIXED BITS (xxxxxxx0xxxxxxxx)
	// u18 not populated

	ROM_REGION( 0x200000, "oki", 0 ) // plain Oki M6295 samples
	ROM_LOAD( "wild_fruit_sp.u15", 0x000000, 0x200000, CRC(9da3e9dd) SHA1(7e447492713549e6be362d4aca6d223dad20771a) ) // M27C160
ROM_END


void igs_m027xa_state::pgm_create_dummy_internal_arm_region()
{
	u16 *temp16 = (u16 *)memregion("maincpu")->base();

	// fill with RX 14
	for (int i = 0; i < 0x4000 / 2; i += 2)
	{
		temp16[i] = 0xff1e;
		temp16[ i +1] = 0xe12f;

	}

	// jump straight to external area
	temp16[(0x0000) / 2] = 0xd088;
	temp16[(0x0002) / 2] = 0xe59f;
	temp16[(0x0004) / 2] = 0x0680;
	temp16[(0x0006) / 2] = 0xe3a0;
	temp16[(0x0008) / 2] = 0xff10;
	temp16[(0x000a) / 2] = 0xe12f;
	temp16[(0x0090) / 2] = 0x0400;
	temp16[(0x0092) / 2] = 0x1000;
}


void igs_m027xa_state::init_hauntedh()
{
	hauntedh_decrypt(machine());
	//m_igs017_igs031->sdwx_gfx_decrypt(machine());
	pgm_create_dummy_internal_arm_region();
}

void igs_m027xa_state::init_crzybugs()
{
	crzybugs_decrypt(machine());
	m_igs017_igs031->sdwx_gfx_decrypt();
	m_igs017_igs031->tarzan_decrypt_sprites(0, 0);
}

void igs_m027xa_state::init_crzybugsj()
{
	crzybugsj_decrypt(machine());
	//qlgs_gfx_decrypt(machine());
	pgm_create_dummy_internal_arm_region();
}

void igs_m027xa_state::init_tripfev()
{
	tripfev_decrypt(machine());
	m_igs017_igs031->sdwx_gfx_decrypt();
	m_igs017_igs031->tarzan_decrypt_sprites(0, 0);
}

void igs_m027xa_state::init_wldfruit()
{
	wldfruit_decrypt(machine());
	//qlgs_gfx_decrypt(machine());
	pgm_create_dummy_internal_arm_region();
}

} // anonymous namespace

// These use the MX10EXAQC (80c51XA from Philips)
// the PCBs are closer to igs_fear.cpp in terms of layout
GAME(  2008, haunthig,  0,        igs_mahjong_xa,     base,     igs_m027xa_state, init_hauntedh,  ROT0, "IGS", "Haunted House (IGS, V109US)", MACHINE_IS_SKELETON ) // IGS FOR V109US 2008 10 14
GAME(  2006, haunthiga, haunthig, igs_mahjong_xa,     base,     igs_m027xa_state, init_hauntedh,  ROT0, "IGS", "Haunted House (IGS, V101US)", MACHINE_IS_SKELETON ) // IGS FOR V101US 2006 08 23

GAMEL( 2009, crzybugs,  0,        igs_mahjong_xa_xor, base,     igs_m027xa_state, init_crzybugs,  ROT0, "IGS", "Crazy Bugs (V204US)", MACHINE_IS_SKELETON, layout_crzybugs ) // IGS FOR V204US 2009 5 19
GAMEL( 2006, crzybugsa, crzybugs, igs_mahjong_xa_xor, base,     igs_m027xa_state, init_crzybugs,  ROT0, "IGS", "Crazy Bugs (V202US)", MACHINE_IS_SKELETON, layout_crzybugs ) // IGS FOR V100US 2006 3 29 but also V202US string
GAMEL( 2005, crzybugsb, crzybugs, igs_mahjong_xa_xor, base,     igs_m027xa_state, init_crzybugs,  ROT0, "IGS", "Crazy Bugs (V200US)", MACHINE_IS_SKELETON, layout_crzybugs ) // FOR V100US 2005 7 20 but also V200US string

GAME(  2007, crzybugsj, crzybugs, igs_mahjong_xa,     base,     igs_m027xa_state, init_crzybugsj, ROT0, "IGS", "Crazy Bugs (V103JP)", MACHINE_IS_SKELETON ) // IGS FOR V101JP 2007 06 08

// XA dump is missing, so XA CPU will crash, disable for now
GAME(  2006, tripfev,   0,        igs_mahjong_xa_xor_disable, base,     igs_m027xa_state, init_tripfev,   ROT0, "IGS", "Triple Fever (V107US)", MACHINE_IS_SKELETON ) // IGS FOR V107US 2006 09 07

GAME(  200?, wldfruit,  0,        igs_mahjong_xa, base,     igs_m027xa_state, init_wldfruit,  ROT0, "IGS", "Wild Fruit (V208US)", MACHINE_IS_SKELETON ) // IGS-----97----V208US
