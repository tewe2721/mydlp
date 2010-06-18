#!/usr/bin/python
# -*- coding: utf-8

# iban.py
# http://kill9.eu/
#
# Copyright (C) 2008 Konstantinos Metaxas 
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published
# by the Free Software Foundation; version 2 only.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

import string

class IBAN(object):

    def __init__(self,iban):
        """Represents an IBAN number.
        iban must be supplied as a string"""
        self.iban = iban.strip().upper()
        #List retrieved from http://en.wikipedia.org/wiki/IBAN
        self.error_list = []
        self.country_length_table = {
                'AD':('Andora',24),
                'AT':('Austria',20),
                'BE':('Belgium',16),
                'BA':('Bosnia and Hetzegovina',20),
                'BG':('Bulgaria',22),
                'HR':('Croatia',21),
                'CY':('Cyprus',28),
                'CZ':('Czech Republic',24),
                'DK':('Denmark',18),
                'EE':('Estonia',20),
                'FO':('Faroe Islands',18),
                'FI':('Finland',18),
                'FR':('France',27),
                'DE':('Germany',22),
                'GI':('Gibraltar',23),
                'GR':('Greece',27),
                'GL':('Greenland',18),
                'HU':('Hungary',28),
                'IS':('Iceland',26),
                'IE':('Republic of Ireland',22),
                'IL':('Israel',23),
                'IT':('Italy',27),
                'LV':('Latvia',21),
                'LI':('Liechtenstein',21),
                'LT':('Lithuania',20),
                'LU':('Luxembourg',20),
                'MK':('FYROM',19),
                'MT':('Malta',31),
                'MC':('Monaco',27),
                'ME':('Montenegro',22),
                'MA':('Morocco',24),
                'NL':('Netherlands',18),
                'NO':('Norway',15),
                'PL':('Poland',28),
                'PT':('Portugal',25),
                'RO':('Romania',24),
                'SM':('San Marino',27),
                'RS':('Serbia',22),
                'SK':('Slovakia',24),
                'SI':('Slovenia',19),
                'ES':('Spain',24),
                'SE':('Sweden',24),
                'CH':('Switzerland',21),
                'TR':('Turkey',26),
                'TN':('Tunisia',24),
                'GB':('United Kingdom',22),

                }

    def is_valid(self):
        """checks if it is a valid IBAN"""

        if not self._is_valid_for_countrycode():
            return False
        if not self._swift_check():
            return False
        return True

    def _is_valid_for_countrycode(self):
        code = self.iban[:2]
        try:
            max_length = self.country_length_table[code][1]
        except KeyError:
            return False
        if len(self.iban) != max_length:
            return False
        else:
            return True

    def _swift_check(self):
        """
        http://en.wikipedia.org/wiki/IBAN
        """
        head_to_tail_iban = self.iban[4:] + self.iban[:4]
        numerical_ascii_map = dict( zip(string.ascii_uppercase,range(10,36)) )

        translated_iban = ""
        for c in head_to_tail_iban:
            if c in numerical_ascii_map:
                translated_iban += str(numerical_ascii_map[c])
            else:
                translated_iban += c

        try:
            integer_iban = int(translated_iban)
        except ValueError:
            return False

        modulo = integer_iban % 97

        if modulo == 1:
            return True
        else:
            return False

