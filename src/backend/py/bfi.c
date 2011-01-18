/*
 * Copyright (C) 2010  Huseyin Ozgur Batur <ozgur@medra.com.tr>
 *
 *    This file is part of MyDLP.
 *
 *    MyDLP is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    (at your option) any later version.
 *
 *    MyDLP is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with MyDLP.  If not, see <http://www.gnu.org/licenses/>.
 */

#include    <fcntl.h>
#include    <stdio.h>
#include    <libelf.h>
#include    <stdlib.h>
#include    <string.h>
#include    <gelf.h>

#define FILE_NOT_FOUND  -1

#define ELF_LIB_ERR     -2
#define UNKNOWN_ARCH    -3

int get_elf_size(char * filepath)
{
    Elf64_Shdr *    shdr64;
    Elf64_Ehdr *    ehdr64;
    Elf32_Shdr *    shdr32;
    Elf32_Ehdr *    ehdr32;

    Elf *        elf;
    Elf_Scn *    scn;
    Elf_Data *    data;

    int        fd;
    unsigned int    cnt;
    long long fsize;
    int unstripped = 0;
    int class;
    int sparc = 0;
    long long  msec_offset = 0;
    int msec_index = 0;

        /* Open the input file */
    if ((fd = open(filepath, O_RDONLY)) == -1){
        return FILE_NOT_FOUND;
    
    }

        /* Get ELF descriptor */
    (void) elf_version(EV_CURRENT);
    if ((elf = elf_begin(fd, ELF_C_READ, NULL)) == NULL){
        return ELF_LIB_ERR;
    
    }

    class = gelf_getclass(elf);

    if (class == ELFCLASS32) {
        
        /* Obtain the .shstrtab data buffer */
        
        if (((ehdr32 = elf32_getehdr(elf)) == NULL) ||
            ((scn = elf_getscn(elf, ehdr32->e_shstrndx)) == NULL) ||
            ((data = elf_getdata(scn, NULL)) == NULL)){
            return ELF_LIB_ERR;
        
        }
        switch(ehdr32->e_machine){

            case EM_386:             /* Intel 80386 */
            case EM_X86_64:          /* AMD x86-64 architecture */
                break;

            case EM_SPARC:           /* SUN SPARC */
            case EM_SPARC32PLUS:     /* Sun's "v8plus" */
            case EM_SPARCV9:         /* SPARC v9 64-bit */            
                sparc = 1;
                break;

            default:
                return UNKNOWN_ARCH;
        }

        /* Traverse input filename, check stripped */
        
        for (cnt = 1, scn = NULL; scn = elf_nextscn(elf, scn); cnt++) {
            if ((shdr32 = elf32_getshdr(scn)) == NULL){
                return ELF_LIB_ERR;
            
            }
            if (shdr32->sh_type == SHT_SYMTAB ){
                unstripped = 1;

            }

            if (shdr32->sh_offset >= msec_offset){
                msec_offset = shdr32->sh_offset;
                msec_index = cnt;

            }            
        }
        
        /* Use biggest offset instead of last offset for Intel unstripped */
        
        if (unstripped && !sparc){    
            scn = elf_getscn(elf, msec_index);
        
        } else {
            scn = elf_getscn(elf, ehdr32->e_shnum - 1);

        }

        if ((shdr32 = elf32_getshdr(scn)) == NULL){
            return ELF_LIB_ERR;

        }

        /* SPARC unstripped same as SPARC stripped*/
        /* Intel unstripped: NO need to add section table size */
        
        if (unstripped && !sparc){
            fsize = shdr32->sh_offset + shdr32->sh_size;
            return fsize;

        /* file size = Last section offset + section table size */    
        /* 4 byte alignment for 32bit */    
        
        } else {
            fsize = shdr32->sh_offset + shdr32->sh_size + ehdr32->e_shnum * ehdr32->e_shentsize;
            int remain = fsize % 4; 
            if (remain !=0){ 
                fsize += 4 - remain;

            }
            return fsize;
        
        }

    
    } else {

        /* Obtain the .shstrtab data buffer */

        if (((ehdr64 = elf64_getehdr(elf)) == NULL) ||
            ((scn = elf_getscn(elf, ehdr64->e_shstrndx)) == NULL) ||
            ((data = elf_getdata(scn, NULL)) == NULL)){
            return ELF_LIB_ERR;
        
        }
        switch(ehdr64->e_machine){

            case EM_386:             /* Intel 80386 */
            case EM_X86_64:          /* AMD x86-64 architecture */
                break;

            case EM_SPARC:           /* SUN SPARC */
            case EM_SPARC32PLUS:     /* Sun's "v8plus" */
            case EM_SPARCV9:         /* SPARC v9 64-bit */            
                sparc = 1;
                break;

            default:
                return UNKNOWN_ARCH;
        
        }

        /* Traverse input filename, check stripped */
        
        for (cnt = 1, scn = NULL; scn = elf_nextscn(elf, scn); cnt++) {
            if ((shdr64 = elf64_getshdr(scn)) == NULL){
                return ELF_LIB_ERR;
            
            }

            if (shdr64->sh_type == SHT_SYMTAB ){
                unstripped = 1;

            }
            if (shdr64->sh_offset >= msec_offset){
                msec_offset = shdr64->sh_offset;
                msec_index = cnt;

            } 
        }
        
        /* Use biggest offset instead of last offset for Intel unstripped */
        
        if (unstripped && !sparc){    
            scn = elf_getscn(elf, msec_index);
        
        } else {
            scn = elf_getscn(elf, ehdr64->e_shnum - 1);

        }
        if ((shdr64 = elf64_getshdr(scn)) == NULL){
            return ELF_LIB_ERR;
        
        }

        /* SPARC unstripped same as SPARC stripped*/
        /* Intel unstripped: NO need to add section table size */
        if (unstripped  && !sparc){
            fsize = shdr64->sh_offset + shdr64->sh_size;
            return fsize;

        /* file size = Last section offset + section table size */    
        /* 8 byte alignment for 64bit */    
        } else {
            fsize = shdr64->sh_offset + shdr64->sh_size + ehdr64->e_shnum * ehdr64->e_shentsize;
            int remain = fsize % 8; 
            if (remain !=0){ 
                fsize += 8 - remain;

            }            
            return fsize;
        }
    }

}

