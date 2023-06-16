// license:BSD-3-Clause
// copyright-holders:AJR
/***************************************************************************

    Harris HD-6120 High-Speed CMOS 12 Bit Microprocessor

    The HD-6120 is a second-generation 12-bit microprocessor developed by
    Harris Semiconductor in conjunction with Digital Equipment Corporation.
    Like its predecessor, Intersil's IM6100 (which Harris second-sourced),
    it is a single-chip implementation of DEC's PDP-8 architecture in
    fully static CMOS with a multiplexed address/data bus. Though HD-6120
    was originally designed to operate at typical frequencies similar to
    those of the IM6100 (its maximum oscillator input was later upgraded
    from 5.1 MHz to 8 MHz), it executes instructions faster largely due to
    increased parallelism; the number of minor states in each machine
    cycle is also considerably more variable on HD-6120.

    HD-6120 also differs from IM6100 in its external interface. Whereas
    IM6100 generates a single narrow LXMAR strobe at the beginning of each
    memory or I/O cycle, followed by strobes that identify the space but
    not the direction of transfer, HD-6120 identifies the space by the
    falling edge of LXMAR, LXPAR or LXDAR and holds it low while generating
    READ and/or WRITE strobes, as well as MEMSEL for memory accesses.
    Accesses to the “switch register” (a data word stored outside the CPU
    itself, as on previous PDP-8s, but is also writable here) are
    identified by READ and WRITE occurring in the absence of LXMAR, LXPAR
    or LXDAR, as are the special register transfer operations. (DATAF is
    asserted for the former, along with DF, and IFETCH for the latter.)
    OUT primarily provides directional control for bus transceivers such
    as HD-6432 and HD-6433 (though 74LS245 may be used instead), but its
    inversion can also be used to provide a wider READ strobe. ACK is used
    to extend READ and WRITE operations while keeping the clock running;
    it may be pulled up to Vcc when not used (as is the case on all of the
    DECmates). LXDAR is also brought low to indicate the data portion of
    auto-indexing cycles, and is held low briefly after RESET to help
    distinguish the power-on IOCLR from the pulse generated by the CAF
    instruction.

    During a minor cycle in which LXMAR, LXPAR or LXDAR goes from high to
    low, either IF or DF is output on the C0, C1 and EMA2 lines together
    with the memory or device address on DX0–DX11. The DATAF output used
    by IM6100 to indicate indirect accesses to data memory may be ignored
    here, though some systems still use it for banking.

    IOT instructions with codes 6000–6007 and 6200–6277 are reserved for
    on-chip functions and do not generate LXDAR when executed. These
    functions include the standard memory extension controls (excluding
    time-sharing modes and the Intersil-exclusive LIF), plus a set of new
    IOTs that can be used to move values of AC and PC to and from two
    independent stacks in memory field 0. SKON, SRQ and GTF are replaced
    by completely different operations in control panel mode, which can
    now be accessed by software using HLT or the new PR0–PR3 traps. Panel
    mode programs may generate indirect accesses to control panel memory
    rather than main memory by setting the Panel Data Flag (PDF).

    For external I/O transfer cycles, HD-6120 performs a WRITE followed by
    an optional READ, whereas IM6100 always performed a read first. During
    completion of the AC write phase, the C0, C1 and SKIP lines are sampled
    to be interpreted appropriately (HD-6120 does not recognize C2 despite
    EMA2 being three-stated at this time), and no data is received from the
    device if C0 is inactive high. (This emulation uses a parallel address
    space to read these flags and defines 1 as low and 0 as high, as these
    and most other signals were on older PDP-8 buses.) DF is also output
    (and DATAF asserted) along with the device address during the LXDAR
    minor cycle, and some peripheral implementations make use of bits
    latched from DF as extra control inputs.

    HD-6120, like IM6100, has a special “control panel” mode, with priority
    over normal interrupts, used to execute supervisory functions from a
    separate memory space that has the same dimensions as main memory but
    is inaccessible from programs executing from there. Due to MAME's
    memory architecture only fully supporting one program space, this
    emulation translates addresses in panel space into the upper extension
    of a 64K-word memory space, which does agree with how the DECmate II
    and DECmate III physically implement it.

    The INTGNT output becomes active low when an interrupt request is
    accepted and returns high at the end of the first external IOT. This
    signal is intended for interrupting devices, specifically the HD-6121
    Input/Output Controller, to respond specially to this first IOT. The
    INTGNT output is suppressed upon entering panel mode, only to become
    active again when execution returns to main memory.

    The HD-6120's 12-bit major registers, besides AC, MQ (which one
    functional diagram more aptly labels the “Accumulator-Adjunct
    Register”), PC and the two stack pointers, include several which are
    only implicitly used in execution: a TEMP register that latches ALU
    outputs, the instruction register IR, and the output latch register OL
    that holds all addresses and data to be output on the DX bus.

    HD-6120 also maintains a group of 3-bit internal registers whose data
    path connects to TEMP. These are used to hold the current memory
    extension fields, their mirrors and various flags. (This emulation
    extends the field registers to 4 bits to include the CTRLFF, PDF and
    PEX flags, which are neither readable nor output directly at any
    time.) These 3-bit registers may be enabled on the C0, C1 and EMA2
    lines at particular times, and the GTF, GCF, PRS, RDF, RIF and RIB
    internal IOTs read various combinations of them into AC. They include:

        MSB             LSB     Output conditions
        -----------------------------------------
        IF0     IF1     IF2     IFETCH, direct operands (except if FZ)
        IB0     IB1     IB2     None (until transferred to IF)
        ISF0    ISF1    ISF2    None
        DF0     DF1     DF2     Indirect operand addressing, IOTs, etc.
        DSF0    DSF1    DSF2    None
        LINK    GT      IEFF    DCA AC writes
        INTREQ* PWRON   0       ISZ result writes
        BTSTRAP PNLTRP  HLTFLG  JMS PC writes

    The GT flag, like MQ, is not used for any specific purpose on the
    HD-6120, unlike the arithmetic extensions of previous PDP-8 CPUs which
    originally implemented them. The INTREQ flag is 1 when the input pin
    is sampled active low and 0 when it is inactive. The PWRON flag is set
    if STRTUP is sampled as VSS at RESET time; it causes the CPU to trap
    into panel mode before executing its first instruction.

    Undefined Group 3 OPRs and internal IOTs have no effect on the
    HD-6120 except that both interrupts and panel requests are blocked
    until after the next instruction. This was apparently provided to
    allow a hypothetical extended arithmetic processor or programmer's
    console to independently decode any number of these instructions and
    reliably trap them by initiating a register transfer operation during
    the immediately following IFETCH cycle. This operation, initiated by
    pulling SKIP low any time an instruction word is being read, causes
    the CPU to disregard that instruction, strobe out AC, flags and MQ,
    then stall indefinitely until SKIP is released, at which point it
    reads AC, LINK, GT, MQ and PC back in.

    As ever with the PDP-8, official documentation numbers MSB as 0 and
    the LSB of a word as 11. This may cause some confusion, since MAME,
    like most computers since the PDP-11, numbers bits the opposite way.

    Known issues:
    * Several instruction timings are likely off by one minor cycle, and
      their cycle-by-cycle sequencing is even more of an educated guess.
      Official documentation is very inconsistent and does not offer
      complete timing information for specific instructions (such as
      exactly when INTREQ is sampled for SRQ or GTF or the duration of
      the IOCLR pulse issued by CAF).
    * Some of the internal IOTs have not been tested thoroughly, the
      stack operations in particular. Their implementation may need to
      be adjusted slightly.
    * The DMAREQ input and DMAGNT output have not been implemented.
    * The RUN/HLT and ACK inputs have not been implemented, though no
      system is known to use either of these.
    * As explained above, auto-indexing cycles are distinguished by the
      assertion of the DATAF signal. If any system uses this output to
      modify memory accesses in some way, the device implementation
      should add whatever hooks and configuration flags are needed.
    * The register transfer operation and special flag outputs have not
      been implemented. Harris's datasheet has no more than one vague
      allusion to the latter feature, and both seem much more likely to
      have been used for IC testing than in any commercial product.

***************************************************************************/

#include "emu.h"
#include "hd6120.h"
#include "pdp8dasm.h"

// device type definition
DEFINE_DEVICE_TYPE(HD6120, hd6120_device, "hd6120", "Harris HD-6120")

ALLOW_SAVE_TYPE(hd6120_device::minor_state)

hd6120_device::hd6120_device(const machine_config &config, const char *tag, device_t *owner, u32 clock)
	: cpu_device(config, HD6120, tag, owner, clock)
	, m_inst_config("instruction", ENDIANNESS_BIG, 16, 16, -1) // 12 data bits
	, m_data_config("data", ENDIANNESS_BIG, 16, 16, -1) // 12 data bits
	, m_io_config("io", ENDIANNESS_BIG, 16, 9, -1) // 12 data bits
	, m_devctl_config("devctl", ENDIANNESS_BIG, 8, 9, 0) // only 3 bits used
	, m_lxmar_callback(*this)
	, m_lxpar_callback(*this)
	, m_lxdar_callback(*this)
	, m_rsr_callback(*this, 0)
	, m_wsr_callback(*this)
	, m_strtup_callback(*this, 1)
	, m_intgnt_callback(*this)
	, m_ioclr_callback(*this)
	, m_pc(0)
	, m_ac(0)
	, m_mq(0)
	, m_sp{0, 0}
	, m_temp(0)
	, m_ir(0)
	, m_if(0)
	, m_ib(0)
	, m_df(0)
	, m_sf(0)
	, m_flags(0)
	, m_pnlflgs(0)
	, m_fz(false)
	, m_iiff(false)
	, m_pwron(false)
	, m_intgnt(false)
	, m_state(minor_state::RESET_1)
	, m_iaddr(0)
	, m_icount(0)
	, m_intreq_input(false)
	, m_cpreq_input(false)
{
	m_inst_config.m_is_octal = true;
	m_data_config.m_is_octal = true;
	m_io_config.m_is_octal = true;
	m_devctl_config.m_is_octal = true; // data might not be logically octal, but addresses sure are
}

std::unique_ptr<util::disasm_interface> hd6120_device::create_disassembler()
{
	return std::make_unique<hd6120_disassembler>();
}

hd6120_device::space_config_vector hd6120_device::memory_space_config() const
{
	if (has_configured_map(AS_DATA))
		return space_config_vector {
			std::make_pair(AS_PROGRAM, &m_inst_config),
			std::make_pair(AS_DATA, &m_data_config),
			std::make_pair(AS_IO, &m_io_config),
			std::make_pair(AS_DEVCTL, &m_devctl_config)
		};
	else
		return space_config_vector {
			std::make_pair(AS_PROGRAM, &m_inst_config),
			std::make_pair(AS_IO, &m_io_config),
			std::make_pair(AS_DEVCTL, &m_devctl_config)
		};
}

u16 hd6120_device::rotate_step(u16 data)
{
	const bool link = BIT(m_flags, 2);
	switch (BIT(m_ir, 1, 3))
	{
	case 0: default:
		// No rotate
		return data;

	case 1: case 6:
		// BSW (twice) or R3L (once)
		return ((data << 3) & 07770) | ((data >> 9) & 7);

	case 2: case 3:
		// RAL (once) or RTL (twice)
		if (BIT(data, 11))
			m_flags |= 4;
		else
			m_flags &= 3;
		return ((data << 1) & 07776) | (link ? 1 : 0);

	case 4: case 5:
		// RAR (once) or RTR (twice)
		if (BIT(data, 0))
			m_flags |= 4;
		else
			m_flags &= 3;
		return ((data >> 1) & 03777) | (link ? 04000 : 0);
	}
}

bool hd6120_device::skip_test() const
{
	bool cond = false;
	if (BIT(m_ir, 6) && m_ac >= 04000) // SMA/SPA
		cond = true;
	else if (BIT(m_ir, 5) && m_ac == 0) // SZA/SNA
		cond = true;
	else if (BIT(m_ir, 4) && BIT(m_flags, 2)) // SNL/SZL
		cond = true;
	if (BIT(m_ir, 3))
		return !cond;
	else
		return cond;
}

u16 hd6120_device::dataf_map(u16 addr) const
{
	if (m_ir >= 04000)
		return u16(m_iiff ? m_ib : m_fz ? 010 : m_if) << 12 | addr;
	else
		return u16(m_df) << 12 | addr;
}

void hd6120_device::next_instruction()
{
	if (m_iiff || m_ib >= 010)
		m_state = minor_state::IFETCH_1;
	else if (m_pwron || m_pnlflgs != 0)
		m_state = minor_state::CPINT_1;
	else if (BIT(m_flags, 0) && m_intreq_input)
		m_state = minor_state::INTGNT_1;
	else
		m_state = minor_state::IFETCH_1;
}

void hd6120_device::transfer_pc(u16 addr)
{
	bool panel_exit = false;
	if (m_iiff)
	{
		if (BIT(m_if & ~m_ib, 3))
		{
			debugger_privilege_hook();
			panel_exit = true;

			// PDF is nominally reset only upon entering panel mode, but has no function outside it
			m_df &= 7;
		}
		m_if = m_ib;
		m_iiff = false;
		m_fz = false;
	}
	m_pc = addr;
	if (m_ib >= 010)
		m_state = minor_state::IFETCH_1;
	else if ((panel_exit ? (m_pnlflgs & 6) : m_pnlflgs) != 0)
		m_state = minor_state::CPINT_1;
	else
	{
		if (m_intgnt && panel_exit)
			m_intgnt_callback(0);
		if (BIT(m_flags, 0) && m_intreq_input)
			m_state = minor_state::INTGNT_1;
		else
			m_state = minor_state::IFETCH_1;
	}
}

void hd6120_device::debug_set_pc(u16 addr)
{
	m_iaddr = addr;
	m_pc = addr & 07777;
	m_if = BIT(addr, 12, 4);
	m_fz = false;
	if (!m_iiff)
	{
		// Fix up IB and DF
		if (BIT(m_if, 3))
			m_ib |= 010;
		else
		{
			m_ib &= 7;
			m_df &= 7;
		}
	}
}

void hd6120_device::debug_update_pc(u16 addr)
{
	m_pc = addr;
	m_iaddr = (m_iaddr & 0170000) | addr;
}

void hd6120_device::device_start()
{
	set_icountptr(m_icount);

	// Bind address spaces
	space(AS_PROGRAM).cache(m_icache);
	space(AS_PROGRAM).specific(m_inst);
	space(has_space(AS_DATA) ? AS_DATA : AS_PROGRAM).specific(m_data);
	space(AS_IO).specific(m_io);
	space(AS_DEVCTL).specific(m_devctl);

	// Register debug state
	using namespace std::placeholders;
	state_add(HD6120_PC, "PC", m_pc, std::bind(&hd6120_device::debug_update_pc, this, _1)).mask(07777).formatstr("%04O");
	state_add(STATE_GENPC, "GENPC", m_iaddr, std::bind(&hd6120_device::debug_set_pc, this, _1)).mask(0177777).formatstr("%06O").noshow();
	state_add(STATE_GENPCBASE, "CURPC", m_iaddr, std::bind(&hd6120_device::debug_set_pc, this, _1)).mask(0177777).formatstr("%06O").noshow();
	state_add(HD6120_IF, "IF", m_if).mask(017).formatstr("%6s");
	state_add(HD6120_IB, "IB", m_ib).mask(017).formatstr("%7s");
	state_add(HD6120_DF, "DF", m_df).mask(017).formatstr("%7s");
	state_add(HD6120_SF, "SF", m_sf).mask(077).formatstr("%02O");
	state_add(HD6120_IIFF, "IIFF", m_iiff);
	state_add(STATE_GENFLAGS, "GENFLAGS", m_flags).mask(7).formatstr("%10s").noshow();
	state_add(HD6120_FLAGS, "FLAGS", m_flags).mask(7);
	state_add(HD6120_PNLFLGS, "PNLFLGS", m_pnlflgs).mask(7);
	state_add(HD6120_PWRON, "PWRON", m_pwron);
	state_add(HD6120_AC, "AC", m_ac).mask(07777).formatstr("%04O");
	state_add(HD6120_MQ, "MQ", m_mq).mask(07777).formatstr("%04O");
	state_add(HD6120_SP1, "SP1", m_sp[0]).mask(07777).formatstr("%04O");
	state_add(HD6120_SP2, "SP2", m_sp[1]).mask(07777).formatstr("%04O");

	// Register save state
	save_item(NAME(m_pc));
	save_item(NAME(m_ac));
	save_item(NAME(m_mq));
	save_item(NAME(m_sp));
	save_item(NAME(m_temp));
	save_item(NAME(m_ir));
	save_item(NAME(m_if));
	save_item(NAME(m_ib));
	save_item(NAME(m_df));
	save_item(NAME(m_sf));
	save_item(NAME(m_flags));
	save_item(NAME(m_pnlflgs));
	save_item(NAME(m_fz));
	save_item(NAME(m_iiff));
	save_item(NAME(m_pwron));
	save_item(NAME(m_intgnt));
	save_item(NAME(m_state));
	save_item(NAME(m_iaddr));
	save_item(NAME(m_oaddr));
	save_item(NAME(m_intreq_input));
	save_item(NAME(m_cpreq_input));
}

void hd6120_device::device_reset()
{
	m_ac = 0;
	m_flags = 0;
	m_pnlflgs = 0;
	m_iiff = false;
	m_fz = false;
	m_pwron = !m_strtup_callback();
	m_if = 0;
	m_ib = 0;
	m_df = 0;
	m_sf = 0;
	m_intgnt = false;
	m_intgnt_callback(1);
	m_ioclr_callback(0);
}

void hd6120_device::execute_run()
{
	do
	{
		switch (m_state)
		{
		case minor_state::RESET_1:
			m_state = minor_state::RESET_2;
			break;

		case minor_state::RESET_2:
			m_ioclr_callback(1);
			m_state = minor_state::RESET_3;
			break;

		case minor_state::RESET_3:
			m_state = minor_state::RESET_4;
			break;

		case minor_state::RESET_4:
			m_temp = 07777;
			m_state = minor_state::RESET_5;
			break;

		case minor_state::RESET_5:
			m_pc = m_temp;
			m_temp = 0;
			next_instruction();
			break;

		case minor_state::IFETCH_1:
			m_ac = m_temp & 07777;
			m_iaddr = u16(m_fz ? 010 : m_if) << 12 | m_pc;
			debugger_instruction_hook(m_iaddr);
			if (m_iaddr >= 0100000)
				m_lxpar_callback(IFETCH, m_iaddr & 077777);
			else
				m_lxmar_callback(IFETCH, m_iaddr);
			m_state = minor_state::IFETCH_2;
			break;

		case minor_state::IFETCH_2:
			m_state = minor_state::IFETCH_3;
			break;

		case minor_state::IFETCH_3:
			m_ir = m_icache.read_word(m_iaddr) & 07777;
			m_temp = m_pc + 1;
			if (m_ir >= 07400)
			{
				if (BIT(m_ir, 0))
					m_state = minor_state::OP3_1;
				else
					m_state = minor_state::OP2_1;
			}
			else if (m_ir >= 07000)
				m_state = minor_state::OP1_1;
			else if (m_ir >= 06000)
				m_state = minor_state::IOT_1;
			else
			{
				m_oaddr = (m_iaddr & (BIT(m_ir, 7) ? 0177600 : 0170000)) | (m_ir & 0177);
				if (BIT(m_ir, 8))
					m_state = minor_state::INDIR_1;
				else if ((m_ir & 07000) == 05000)
					m_state = minor_state::JMP_1;
				else
					m_state = minor_state::EXEC_1;
			}
			break;

		case minor_state::INDIR_1:
			m_pc = m_temp & 07777;
			if (m_oaddr >= 0100000)
				m_lxpar_callback(INSTF, m_oaddr & 077777);
			else
				m_lxmar_callback(INSTF, m_oaddr);
			m_state = minor_state::INDIR_2;
			break;

		case minor_state::INDIR_2:
			if ((m_oaddr & 07770) == 0010)
			{
				// Begin auto-indexing
				m_state = minor_state::INDIR_3A;
			}
			else
				m_state = minor_state::INDIR_3;
			break;

		case minor_state::INDIR_3:
			m_oaddr = dataf_map(m_inst.read_word(m_oaddr) & 07777);
			m_temp = m_pc;
			if ((m_ir & 07000) == 05000)
				m_state = minor_state::JMP_1;
			else
				m_state = minor_state::EXEC_1;
			break;

		case minor_state::INDIR_3A:
			m_temp = m_inst.read_word(m_oaddr) + 1;
			m_state = minor_state::INDIR_4;
			break;

		case minor_state::INDIR_4:
			m_state = minor_state::INDIR_5;
			break;

		case minor_state::INDIR_5:
			m_inst.write_word(m_oaddr, m_temp & 07777);
			m_oaddr = dataf_map(m_temp & 07777);
			m_temp = m_pc;
			if ((m_ir & 07000) == 05000)
				m_state = minor_state::JMP_1;
			else
				m_state = minor_state::EXEC_1;
			break;

		case minor_state::EXEC_1:
		{
			const bool dataf = BIT(m_ir, 8) && m_ir < 04000;
			if (m_oaddr >= 0100000)
				m_lxpar_callback(dataf ? DATAF : INSTF, m_oaddr & 077777);
			else
				m_lxmar_callback(dataf ? DATAF : INSTF, m_oaddr);
			m_pc = m_temp & 07777;
			if (m_ir >= 03000)
				m_state = minor_state::DEP_2;
			else
				m_state = minor_state::EXEC_2;
			break;
		}

		case minor_state::EXEC_2:
			m_state = minor_state::EXEC_3;
			break;

		case minor_state::EXEC_3:
			if (BIT(m_ir, 8))
				m_temp = m_data.read_word(m_oaddr) & 07777;
			else
				m_temp = m_inst.read_word(m_oaddr) & 07777;
			if (m_ir >= 02000)
				m_state = minor_state::ISZ_4;
			else if (m_ir >= 01000)
				m_state = minor_state::TAD_4;
			else
				m_state = minor_state::AND_4;
			break;

		case minor_state::DEP_2:
			m_state = minor_state::DEP_3;
			break;

		case minor_state::DEP_3:
			m_temp = m_oaddr + 1;
			if (m_ir >= 04000)
				m_state = minor_state::JMS_4;
			else
				m_state = minor_state::DCA_4;
			break;

		case minor_state::AND_4:
			m_temp &= m_ac;
			next_instruction();
			break;

		case minor_state::TAD_4:
			m_temp += m_ac;
			if (m_temp >= 010000)
				m_flags ^= 4; // LINK is complemented upon carry out
			next_instruction();
			break;

		case minor_state::ISZ_4:
			++m_temp;
			m_state = minor_state::ISZ_5;
			break;

		case minor_state::ISZ_5:
			m_state = minor_state::ISZ_6;
			break;

		case minor_state::ISZ_6:
			if (BIT(m_ir, 8))
				m_data.write_word(m_oaddr, m_temp & 07777);
			else
				m_inst.write_word(m_oaddr, m_temp & 07777);
			if (m_temp < 010000)
				next_instruction();
			else
				m_state = minor_state::ISZ_7;
			m_temp = m_ac;
			break;

		case minor_state::ISZ_7:
			m_temp = m_pc + 1;
			m_state = minor_state::ISZ_8;
			break;

		case minor_state::ISZ_8:
			m_pc = m_temp & 07777;
			m_temp = m_ac;
			next_instruction();
			break;

		case minor_state::DCA_4:
			if (BIT(m_ir, 8))
				m_data.write_word(m_oaddr, m_ac);
			else
				m_inst.write_word(m_oaddr, m_ac);
			m_temp = 0;
			next_instruction();
			break;

		case minor_state::JMS_4:
			m_icache.write_word(m_oaddr, m_pc);
			transfer_pc(m_temp & 07777);
			m_temp = m_ac;
			break;

		case minor_state::JMP_1:
			transfer_pc(m_oaddr & 07777);
			m_temp = m_ac;
			break;

		case minor_state::OP1_1:
			m_pc = m_temp & 07777;
			m_temp = (BIT(m_ir, 7) ? 0 : m_ac) ^ (BIT(m_ir, 5) ? 07777 : 0); // CLA and/or CMA
			if (BIT(m_ir, 6))
				m_flags &= 3; // CLL
			if (BIT(m_ir, 4))
				m_flags ^= 4; // CML
			m_state = minor_state::OP1_2;
			break;

		case minor_state::OP1_2:
			if (BIT(m_ir, 0))
			{
				++m_temp; // IAC
				if (m_temp == 010000)
				{
					m_flags ^= 4; // LINK is complemented upon carry out
					m_temp = 0;
				}
			}
			m_state = minor_state::OP1_3;
			break;

		case minor_state::OP1_3:
			m_temp = rotate_step(m_temp);
			if (BIT(m_ir, 1))
				m_state = minor_state::OP1_4;
			else
				next_instruction();
			break;

		case minor_state::OP1_4:
			m_state = minor_state::OP1_5;
			break;

		case minor_state::OP1_5:
			m_temp = rotate_step(m_temp);
			next_instruction();
			break;

		case minor_state::OP2_1:
			if (skip_test())
				++m_temp;
			if (BIT(m_ir, 2))
				m_state = minor_state::OSR_2;
			else
				m_state = minor_state::OP2_2;
			break;

		case minor_state::OP2_2:
			m_pc = m_temp & 07777;
			m_temp = 0;
			m_state = minor_state::OP2_3;
			break;

		case minor_state::OP2_3:
			if (BIT(m_ir, 1))
				m_pnlflgs |= 1; // Set HLTFLG
			m_state = minor_state::OP2_4;
			break;

		case minor_state::OP2_4:
			m_temp |= BIT(m_ir, 7) ? 0 : m_ac;
			next_instruction();
			break;

		case minor_state::OSR_2:
			m_pc = m_temp & 07777;
			m_state = minor_state::OSR_3;
			break;

		case minor_state::OSR_3:
			if (m_rsr_callback.isunset())
			{
				logerror("%06o: SR read (IR = %04o)\n", m_iaddr, m_ir);
				m_temp = 0;
			}
			else
				m_temp = m_rsr_callback(m_df & 7);
			m_state = minor_state::OP2_3;
			break;

		case minor_state::OP3_1:
			m_pc = m_temp & 07777;
			m_temp = BIT(m_ir, 7) ? 0 : m_ac;
			m_state = minor_state::OP3_2;
			break;

		case minor_state::OP3_2:
			m_state = minor_state::OP3_3;
			break;

		case minor_state::OP3_3:
			if (BIT(m_ir, 4))
			{
				if (BIT(m_ir, 6))
					std::swap(m_temp, m_mq);
				else
				{
					// MQL always clears AC
					m_mq = m_temp;
					m_temp = 0;
				}
			}
			else if (BIT(m_ir, 6))
				m_temp |= m_mq;
			if ((m_ir & 0056) != 0)
				m_state = minor_state::IFETCH_1; // Interrupts conditionally blocked
			else
				next_instruction();
			break;

		case minor_state::IOT_1:
			m_pc = m_temp & 07777;
			if (m_ir >= 06010 && (m_ir & 0700) != 0200)
				m_state = minor_state::EXTIOT_1;
			else switch (m_ir & 0777)
			{
			case 0000:
				if (m_if >= 010)
					m_state = minor_state::PRS_1;
				else
					m_state = minor_state::SKON_1;
				break;

			case 0001: case 0002:
				m_state = minor_state::IEN_1;
				break;

			case 0003:
				if (m_if >= 010)
					m_state = minor_state::PGO_1;
				else
					m_state = minor_state::SRQ_1;
				break;

			case 0004:
				if (m_if >= 010)
					m_state = minor_state::PEX_1;
				else
					m_state = minor_state::GTF_1;
				break;

			case 0005:
				m_state = minor_state::RTF_1;
				break;

			case 0006:
				m_state = minor_state::SGT_1;
				break;

			case 0007:
				m_state = minor_state::CAF_1;
				break;

			case 0201: case 0202: case 0203:
			case 0211: case 0212: case 0213:
			case 0221: case 0222: case 0223:
			case 0231: case 0232: case 0233:
			case 0241: case 0242: case 0243:
			case 0251: case 0252: case 0253:
			case 0261: case 0262: case 0263:
			case 0271: case 0272: case 0273:
				m_state = minor_state::CFIELD_1;
				break;

			case 0205: case 0245:
				m_state = minor_state::PPC_1;
				break;

			case 0206: case 0216: case 0226: case 0236:
				m_state = minor_state::PRQ_1;
				break;

			case 0207: case 0227:
				m_state = minor_state::RSP_1;
				break;

			case 0214: case 0224:
				m_state = minor_state::RFIELD_1;
				break;

			case 0215: case 0255:
				m_state = minor_state::PAC_1;
				break;

			case 0217: case 0237:
				m_state = minor_state::LSP_1;
				break;

			case 0225: case 0265:
				m_state = minor_state::RTN_1;
				break;

			case 0234:
				m_state = minor_state::RIB_1;
				break;

			case 0235: case 0275:
				m_state = minor_state::POP_1;
				break;

			case 0244:
				m_state = minor_state::RMF_1;
				break;

			case 0246:
				m_state = minor_state::WSR_1;
				break;

			case 0256:
				m_state = minor_state::GCF_1;
				break;

			case 0266: case 0276:
				m_state = minor_state::SPD_1;
				break;

			default:
				m_state = minor_state::IOT_2;
				break;
			}
			break;

		case minor_state::IOT_2:
			logerror("%06o: Undefined internal IOT (IR=%04o, AC=%04o)\n", m_iaddr, m_ir, m_ac);
			m_temp = m_ac;
			m_state = minor_state::IFETCH_1;
			break;

		case minor_state::SKON_1:
			m_temp = m_pc + (m_flags & 1);
			m_flags &= 6;
			m_state = minor_state::SKON_2;
			break;

		case minor_state::SKON_2:
			m_pc = m_temp;
			m_state = minor_state::SKON_3;
			break;

		case minor_state::SKON_3:
			m_temp = m_ac;
			next_instruction();
			break;

		case minor_state::IEN_1:
			m_flags = (m_flags & 6) | (m_ir & 0001);
			m_state = minor_state::IEN_2;
			break;

		case minor_state::IEN_2:
			m_temp = m_ac;
			m_state = minor_state::IFETCH_1; // Interrupts are blocked
			break;

		case minor_state::SRQ_1:
			m_temp = m_pc + (m_intreq_input ? 1 : 0);
			m_state = minor_state::SKON_2;
			break;

		case minor_state::GTF_1:
			m_state = minor_state::GTF_2;
			break;

		case minor_state::GTF_2:
			m_state = minor_state::GTF_3;
			break;

		case minor_state::GTF_3:
			m_temp = m_sf;
			m_state = minor_state::GTF_4;
			break;

		case minor_state::GTF_4:
			m_temp |= u16(m_flags & 6) << 9 | 0200; // 1 is loaded into bit 4 instead of IEFF
			m_state = minor_state::GTF_5;
			break;

		case minor_state::GTF_5:
			if (m_intreq_input)
				m_temp |= 01000;
			if (m_pwron)
				m_temp |= 0400;
			next_instruction();
			break;

		case minor_state::RTF_1:
			m_temp = m_ac;
			m_flags = (m_temp & 06000) >> 9 | (m_temp & 0200) >> 7;
			m_state = minor_state::RTF_2;
			break;

		case minor_state::RTF_2:
			m_df = m_temp & 0007;
			m_state = minor_state::RTF_3;
			break;

		case minor_state::RTF_3:
			m_ib = (m_temp & 0070) >> 3;
			m_iiff = true;
			m_state = minor_state::RTF_4;
			break;

		case minor_state::RTF_4:
			m_temp = 0;
			next_instruction();
			break;

		case minor_state::SGT_1:
			m_temp = m_pc + (BIT(m_flags, 1) ? 1 : 0);
			m_state = minor_state::SKON_2;
			break;

		case minor_state::CAF_1:
			m_temp = 0;
			m_state = minor_state::CAF_2;
			break;

		case minor_state::CAF_2:
			m_flags = 0; // LINK, GT and IEFF are cleared
			m_ioclr_callback(0);
			m_state = minor_state::CAF_3;
			break;

		case minor_state::CAF_3:
			m_ioclr_callback(1);
			next_instruction();
			break;

		case minor_state::PRS_1:
			m_state = minor_state::PRS_2;
			break;

		case minor_state::PRS_2:
			m_state = minor_state::PRS_3;
			break;

		case minor_state::PRS_3:
			m_state = minor_state::PRS_4;
			break;

		case minor_state::PRS_4:
			m_temp = u16(m_pnlflgs & 6) << 9 | (m_pnlflgs & 1) << 7;
			if (m_intreq_input)
				m_temp |= 01000;
			if (m_pwron)
				m_temp |= 0400;
			m_pnlflgs &= m_temp >= 04000 ? 1 : 5;
			m_pwron = false;
			m_state = minor_state::IFETCH_1;
			break;

		case minor_state::PGO_1:
			m_pnlflgs &= 6; // Clear HLTFLG
			m_state = minor_state::SKON_3;
			break;

		case minor_state::PEX_1:
			m_temp = m_ac;
			m_state = minor_state::PEX_2;
			break;

		case minor_state::PEX_2:
			m_ib &= 7;
			m_pnlflgs &= 5; // Clear PNLTRP
			m_pwron = false;
			m_iiff = true;
			m_state = minor_state::IFETCH_1;
			break;

		case minor_state::CFIELD_1:
			m_state = minor_state::CFIELD_2;
			break;

		case minor_state::CFIELD_2:
			m_temp = m_ac;
			if (BIT(m_ir, 1))
			{
				m_ib = (m_ib & 010) | BIT(m_ir, 3, 3);
				m_iiff = true;
			}
			if (BIT(m_ir, 0))
				m_df = (m_df & 010) | BIT(m_ir, 3, 3);
			next_instruction();
			break;

		case minor_state::RFIELD_1:
			m_state = minor_state::RFIELD_2;
			break;

		case minor_state::RFIELD_2:
			m_temp = m_ac | (BIT(m_ir, 3) ? m_df & 7 : m_if & 7) << 3;
			next_instruction();
			break;

		case minor_state::RIB_1:
			m_state = minor_state::RIB_2;
			break;

		case minor_state::RIB_2:
			m_temp = m_ac | m_sf;
			next_instruction();
			break;

		case minor_state::RMF_1:
			m_state = minor_state::RMF_2;
			break;

		case minor_state::RMF_2:
			m_temp = m_sf;
			m_state = minor_state::RMF_3;
			break;

		case minor_state::RMF_3:
			m_df = (m_df & 010) | (m_temp & 7);
			m_state = minor_state::RMF_4;
			break;

		case minor_state::RMF_4:
			m_ib = (m_ib & 010) | ((m_temp >> 3) & 7);
			m_iiff = true;
			m_state = minor_state::SKON_3;
			break;

		case minor_state::PRQ_1:
			if (m_if < 010)
				m_pnlflgs |= 2; // Set PNLTRP
			m_state = minor_state::SKON_3;
			break;

		case minor_state::WSR_1:
			m_state = minor_state::WSR_2;
			break;

		case minor_state::WSR_2:
			// WSR may be used from main memory as well as panel memory; DECmates protect it using external circuitry
			m_wsr_callback(m_df & 7, m_ac);
			m_state = minor_state::RTF_4;
			break;

		case minor_state::GCF_1:
			m_state = minor_state::GCF_2;
			break;

		case minor_state::GCF_2:
			m_temp = m_df & 7;
			m_state = minor_state::GCF_3;
			break;

		case minor_state::GCF_3:
			m_temp |= (m_if & 7) << 3;
			m_state = minor_state::GCF_4;
			break;

		case minor_state::GCF_4:
			m_temp |= u16(m_flags & 6) << 9 | (m_flags & 1) << 7;
			m_state = minor_state::GTF_5;
			break;

		case minor_state::SPD_1:
			m_temp = m_ac;
			if (BIT(m_ir, 3) && m_if >= 010)
				m_df |= 010;
			else
				m_df &= 7;
			next_instruction();
			break;

		case minor_state::PPC_1:
			m_temp = m_pc + 1;
			m_state = minor_state::PPC_2;
			break;

		case minor_state::PPC_2:
			m_oaddr = u16(m_if & 010) << 12 | m_sp[BIT(m_ir, 5)];
			m_state = minor_state::PPC_3;
			break;

		case minor_state::PPC_3:
			if (m_if >= 010)
				m_lxpar_callback(INSTF, m_sp[BIT(m_ir, 5)]);
			else
				m_lxmar_callback(INSTF, m_sp[BIT(m_ir, 5)]);
			m_state = minor_state::PPC_4;
			break;

		case minor_state::PPC_4:
			m_state = minor_state::PPC_5;
			break;

		case minor_state::PPC_5:
			m_inst.write_word(m_oaddr, m_temp);
			m_temp = m_sp[BIT(m_ir, 5)] - 1;
			m_state = minor_state::PPC_6;
			break;

		case minor_state::PPC_6:
			m_sp[BIT(m_ir, 5)] = m_temp & 07777;
			m_temp = m_ac;
			next_instruction();
			break;

		case minor_state::PAC_1:
			m_oaddr = u16(m_if & 010) << 12 | m_sp[BIT(m_ir, 5)];
			m_state = minor_state::PAC_2;
			break;

		case minor_state::PAC_2:
			if (m_if >= 010)
				m_lxpar_callback(INSTF, m_sp[BIT(m_ir, 5)]);
			else
				m_lxmar_callback(INSTF, m_sp[BIT(m_ir, 5)]);
			m_state = minor_state::PAC_3;
			break;

		case minor_state::PAC_3:
			m_temp = m_ac;
			m_state = minor_state::PPC_5;
			break;

		case minor_state::RTN_1:
			m_temp = m_sp[BIT(m_ir, 5)] + 1;
			m_state = minor_state::RTN_2;
			break;

		case minor_state::RTN_2:
			m_oaddr = u16(m_ib & 010) << 12 | (m_temp & 07777);
			m_state = minor_state::RTN_3;
			break;

		case minor_state::RTN_3:
			m_sp[BIT(m_ir, 5)] = m_temp & 07777;
			m_state = minor_state::RTN_4;
			break;

		case minor_state::RTN_4:
			m_oaddr = u16(m_ib) << 12 | m_inst.read_word(m_sp[BIT(m_ir, 5)]);
			m_state = minor_state::JMP_1;
			break;

		case minor_state::POP_1:
			m_temp = m_sp[BIT(m_ir, 5)] + 1;
			m_state = minor_state::POP_2;
			break;

		case minor_state::POP_2:
			m_oaddr = u16(m_if & 010) << 12 | (m_temp & 07777);
			m_state = minor_state::POP_3;
			break;

		case minor_state::POP_3:
			m_sp[BIT(m_ir, 5)] = m_temp & 07777;
			m_state = minor_state::POP_4;
			break;

		case minor_state::POP_4:
			m_ac = m_inst.read_word(m_sp[BIT(m_ir, 5)]);
			m_state = minor_state::SKON_3;
			break;

		case minor_state::RSP_1:
			m_state = minor_state::RSP_2;
			break;

		case minor_state::RSP_2:
			m_temp = m_sp[BIT(m_ir, 4)];
			next_instruction();
			break;

		case minor_state::LSP_1:
			m_sp[BIT(m_ir, 4)] = m_ac;
			m_state = minor_state::RTF_4;
			break;

		case minor_state::EXTIOT_1:
			m_lxdar_callback(DATAF, u16(m_df & 7) << 12 | m_ir);
			m_state = minor_state::EXTIOT_2;
			break;

		case minor_state::EXTIOT_2:
			m_temp = m_pc + 1;
			m_state = minor_state::EXTIOT_3;
			break;

		case minor_state::EXTIOT_3:
		{
			// C0, C1 and SKIP are sampled at the rising edge of WRITE
			// The control flags are read first here, since writes can and do change them
			// (see esp. 6366 on DECmate II, where the change depends on the data written)
			u8 devctl = m_devctl.read_byte(m_ir & 0777);
			m_io.write_word(m_ir & 0777, m_ac);
			if ((devctl & SKIP) != 0)
				m_pc = m_temp & 07777;
			m_temp = (devctl & C0) != 0 ? 0 : m_ac;
			if ((devctl & C1) != 0)
				m_state = minor_state::EXTIOT_4R;
			else
				m_state = minor_state::EXTIOT_4;
			break;
		}

		case minor_state::EXTIOT_4:
			// IOTs take one fewer minor cycle if no read
			m_state = minor_state::EXTIOT_5;
			break;

		case minor_state::EXTIOT_4R:
			m_state = minor_state::EXTIOT_5R;
			break;

		case minor_state::EXTIOT_5R:
			m_temp |= m_io.read_word(m_ir & 0777);
			m_state = minor_state::EXTIOT_5;
			break;

		case minor_state::EXTIOT_5:
			if (m_intgnt && m_if < 010)
			{
				m_intgnt = false;
				m_intgnt_callback(1);
			}
			next_instruction();
			break;

		case minor_state::INTGNT_1:
			m_ac = m_temp & 07777;
			(void)standard_irq_callback(INTREQ_LINE, m_if << 12 | m_pc);
			m_intgnt = true;
			m_intgnt_callback(0);
			m_flags &= 6;
			m_sf = m_if << 3 | m_df; // Save fields
			m_if = 0;
			m_ib = 0;
			m_df = 0;
			m_oaddr = 0;
			m_lxmar_callback(INSTF, 0);
			m_ir = 04000;
			m_state = minor_state::DEP_2;
			break;

		case minor_state::CPINT_1:
			m_ac = m_temp & 07777;
			debugger_privilege_hook();
			if (BIT(m_pnlflgs, 2))
				(void)standard_irq_callback(CPREQ_LINE, m_if << 12 | m_pc);
			if (m_intgnt)
				m_intgnt_callback(1);
			m_if |= 010;
			m_ib |= 010;
			m_fz = true;
			m_oaddr = 0100000;
			m_lxpar_callback(INSTF, 0);
			m_state = minor_state::CPINT_2;
			break;

		case minor_state::CPINT_2:
			m_temp = 07777;
			m_state = minor_state::JMS_4;
			break;
		}
	} while (--m_icount > 0);
}

void hd6120_device::execute_set_input(int linenum, int state)
{
	switch (linenum)
	{
	case INTREQ_LINE:
		m_intreq_input = state != CLEAR_LINE;
		break;

	case CPREQ_LINE:
		// Falling-edge active Schmitt-trigger input
		if (!m_cpreq_input && state != CLEAR_LINE)
			m_pnlflgs |= 4; // Set BTSTRP
		m_cpreq_input = state != CLEAR_LINE;
		break;
	}
}

void hd6120_device::state_string_export(const device_state_entry &entry, std::string &str) const
{
	switch (entry.index())
	{
	case STATE_GENFLAGS:
		str = util::string_format("%s %s %s", BIT(m_flags, 2) ? "LINK" : "----", BIT(m_flags, 1) ? "GT" : "--", BIT(m_flags, 0) ? "IE" : "--");
		break;

	case HD6120_IF:
		if (BIT(m_if, 3))
			str = util::string_format("%o (%s)", m_if & 7, m_fz ? "FZ" : "CP");
		else
			str = util::string_format("%o     ", m_if);
		break;

	case HD6120_DF:
		if (BIT(m_df, 3))
			str = util::string_format("%o (PDF)", m_df & 7);
		else
			str = util::string_format("%o      ", m_df);
		break;

	case HD6120_IB:
		if (BIT(m_ib, 3))
			str = util::string_format("%o (CP) ", m_ib & 7);
		else if (BIT(m_if, 3))
			str = util::string_format("%o (PEX)", m_ib);
		else
			str = util::string_format("%o      ", m_ib);
		break;
	}
}
