/*
 * Created by Geoff Crewe.
 *
 * Copyright 2011 MIT Haystack Observatory 
 *  
 * This file is part of mark6.
 *
 * mark6 is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * mark6 is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with mark6.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

/*
 * $Id: bmr_files.h 984 2011-02-08 17:02:31Z gbc $
 *
 * BMR File Conventions
 */

#ifndef bmr_files_h
#define bmr_files_h

/*
 * The *RAID types involve just one fd for reading/writing,
 * the *FILES types open multiple files for reading/writing.
 *
 * At present, only BMR_TYPE_RAID0 is supported and the *_type
 * members of save and load are not used.
 */
#define BMR_TYPE_FILES     0
#define BMR_TYPE_RAID0     1
#define BMR_TYPE_RAID5     2

/*
 * The mem_type specifies how the packet is stored in memory.
 *
 * For BMR_MEM_SEQN memory has 8B-seqn and packet (the original
 *                  implementation)
 * For BMR_MEM_EDVS memory has the packet with the seqn
 *                  packed into the extended (VDIF) data header
 */
#define BMR_MEM_SEQN       0
#define BMR_MEM_EDVS       1

/*
 * The file_fmt member specifies the saved packet structure.
 *
 * seqn is the bmr sequence number
 * psn is the vdif/m5b psn
 * pkt is the vdif/m5b header + data packet
 *
 * These assume the PSN was sent
 * For BMR_FILE_RAW the seqn+psn+pkt is transferred.
 * For BMR_FILE_PSN the psn+pkt is transferred with a sequence number.
 * For BMR_FILE_PKT the pkt alone is transferred.
 *
 * These assume no PSN was sent
 * For BMR_FILE_VDF the packet alone is transferred.
 * For BMR_FILE_VDE the packet has sequence numbers in its header.
 * For BMR_FILE_VDX experimental hacking version of BMR_FILE_VDE/VDF.
 */
#define BMR_FILE_RAW       0
#define BMR_FILE_PSN       1
#define BMR_FILE_PKT       2
#define BMR_FILE_VDF       3
#define BMR_FILE_VDE       4
#define BMR_FILE_VDX       5
#define BMR_FILE_NUM       6

/* in bmr_scan.c */
extern char *bmr_file_suffix_fmt[BMR_FILE_NUM];

#endif /* bmr_files_h */

/*
 * eof
 */
