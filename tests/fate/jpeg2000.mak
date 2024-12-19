# The following tests are based on the conformance suite specified in
# Rec. ITU-T T.803 | ISO/IEC 15444-4 available at the following URLs:
# * https://gitlab.com/wg1/htj2k-codestreams
# * https://www.itu.int/rec/T-REC-T.803/en
# * https://www.iso.org/standard/81574.html
#
# Notes:
# * p0_06.j2k is not included because it uses a pixel format that is not
#   supported (4:2:2:1)
# * p0_10.j2k is not included because errors are emitted during decoding and
#   there are significant deviations from the reference image in the bottom-left
#   quadrant
# * p0_13.j2k is not included because it uses a pixel format that is not
#   supported (257 color channels)
# * p0_04.j2k and p0_05.j2k exceed the error thresholds specified in the
#   conformance suite
# * p0_09.j2k matches the reference image exactly when bitexact is not used, but
#   exceed the error thresholds specified in the conformance suite when bitexact
#   is used

FATE_JPEG2000DEC += fate-jpeg2000dec-p0_01
fate-jpeg2000dec-p0_01: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/codestreams_profile0/p0_01.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-p0_02
fate-jpeg2000dec-p0_02: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/codestreams_profile0/p0_02.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-p0_03
fate-jpeg2000dec-p0_03: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/codestreams_profile0/p0_03.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-p0_04
fate-jpeg2000dec-p0_04: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/codestreams_profile0/p0_04.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-p0_05
fate-jpeg2000dec-p0_05: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/codestreams_profile0/p0_05.j2k

FATE_JPEG2000DEC-$(CONFIG_SCALE_FILTER) += fate-jpeg2000dec-p0_07
fate-jpeg2000dec-p0_07: CMD = framecrc -flags +bitexact -auto_conversion_filters -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/codestreams_profile0/p0_07.j2k -pix_fmt rgb48le

FATE_JPEG2000DEC-$(CONFIG_SCALE_FILTER) += fate-jpeg2000dec-p0_08
fate-jpeg2000dec-p0_08: CMD = framecrc -flags +bitexact -auto_conversion_filters -lowres 1 -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/codestreams_profile0/p0_08.j2k -pix_fmt rgb48le

FATE_JPEG2000DEC += fate-jpeg2000dec-p0_09
fate-jpeg2000dec-p0_09: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/codestreams_profile0/p0_09.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-p0_10
fate-jpeg2000dec-p0_10: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/codestreams_profile0/p0_10.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-p0_11
fate-jpeg2000dec-p0_11: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/codestreams_profile0/p0_11.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-p0_12
fate-jpeg2000dec-p0_12: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/codestreams_profile0/p0_12.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-p0_14
fate-jpeg2000dec-p0_14: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/codestreams_profile0/p0_14.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-p0_15
fate-jpeg2000dec-p0_15: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/codestreams_profile0/p0_15.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-p0_16
fate-jpeg2000dec-p0_16: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/codestreams_profile0/p0_16.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-p1_01
fate-jpeg2000dec-p1_01: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/codestreams_profile1/p1_01.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-p1_02
fate-jpeg2000dec-p1_02: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/codestreams_profile1/p1_02.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-p1_03
fate-jpeg2000dec-p1_03: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/codestreams_profile1/p1_03.j2k

# The MSE and Peak Error values are 16x those found in ISO/IEC 15444-4: the latter assume 12-bit samples while FFMPEG decodes
# to 16-bit samples
FATE_JPEG2000DEC += fate-jpeg2000dec-p1_04
fate-jpeg2000dec-p1_04: CMD = mse_peak_error $(TARGET_SAMPLES)/jpeg2000/itu-iso/codestreams_profile1/p1_04.j2k \
																							$(TARGET_SAMPLES)/jpeg2000/itu-iso/codestreams_profile1/c1p1_04-0.j2c \
																							49280 9984

FATE_JPEG2000DEC += fate-jpeg2000dec-p1_05
fate-jpeg2000dec-p1_05: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/codestreams_profile1/p1_05.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-p1_06
fate-jpeg2000dec-p1_06: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/codestreams_profile1/p1_06.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-hifi_ht1_02
fate-jpeg2000dec-hifi_ht1_02: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/codestreams_profile1/hifi_ht1_02.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-hifi_p1_02
fate-jpeg2000dec-hifi_p1_02: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/codestreams_profile1/hifi_p1_02.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-ds0_ht_01_b11
fate-jpeg2000dec-ds0_ht_01_b11: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/htj2k_bsets_profile0/p0_01_bset/ds0_ht_01_b11.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-ds0_ht_02_b11
fate-jpeg2000dec-ds0_ht_02_b11: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/htj2k_bsets_profile0/p0_02_bset/ds0_ht_02_b11.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-ds0_ht_02_b12
fate-jpeg2000dec-ds0_ht_02_b12: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/htj2k_bsets_profile0/p0_02_bset/ds0_ht_02_b12.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-ds0_ht_03_b11
fate-jpeg2000dec-ds0_ht_03_b11: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/htj2k_bsets_profile0/p0_03_bset/ds0_ht_03_b11.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-ds0_ht_03_b14
fate-jpeg2000dec-ds0_ht_03_b14: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/htj2k_bsets_profile0/p0_03_bset/ds0_ht_03_b14.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-ds0_ht_04_b11
fate-jpeg2000dec-ds0_ht_04_b11: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/htj2k_bsets_profile0/p0_04_bset/ds0_ht_04_b11.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-ds0_ht_04_b12
fate-jpeg2000dec-ds0_ht_04_b12: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/htj2k_bsets_profile0/p0_04_bset/ds0_ht_04_b12.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-ds0_ht_05_b11
fate-jpeg2000dec-ds0_ht_05_b11: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/htj2k_bsets_profile0/p0_05_bset/ds0_ht_05_b11.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-ds0_ht_05_b12
fate-jpeg2000dec-ds0_ht_05_b12: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/htj2k_bsets_profile0/p0_05_bset/ds0_ht_05_b12.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-ds0_ht_07_b11
fate-jpeg2000dec-ds0_ht_07_b11: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/htj2k_bsets_profile0/p0_07_bset/ds0_ht_07_b11.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-ds0_ht_07_b15
fate-jpeg2000dec-ds0_ht_07_b15: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/htj2k_bsets_profile0/p0_07_bset/ds0_ht_07_b15.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-ds0_ht_07_b16
fate-jpeg2000dec-ds0_ht_07_b16: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/htj2k_bsets_profile0/p0_07_bset/ds0_ht_07_b16.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-ds0_ht_08_b11
fate-jpeg2000dec-ds0_ht_08_b11: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/htj2k_bsets_profile0/p0_08_bset/ds0_ht_08_b11.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-ds0_ht_08_b15
fate-jpeg2000dec-ds0_ht_08_b15: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/htj2k_bsets_profile0/p0_08_bset/ds0_ht_08_b15.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-ds0_ht_08_b16
fate-jpeg2000dec-ds0_ht_08_b16: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/htj2k_bsets_profile0/p0_08_bset/ds0_ht_08_b16.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-ds0_ht_09_b11
fate-jpeg2000dec-ds0_ht_09_b11: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/htj2k_bsets_profile0/p0_09_bset/ds0_ht_09_b11.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-ds0_ht_10_b11
fate-jpeg2000dec-ds0_ht_10_b11: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/htj2k_bsets_profile0/p0_10_bset/ds0_ht_10_b11.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-ds0_ht_11_b10
fate-jpeg2000dec-ds0_ht_11_b10: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/htj2k_bsets_profile0/p0_11_bset/ds0_ht_11_b10.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-ds0_ht_12_b11
fate-jpeg2000dec-ds0_ht_12_b11: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/htj2k_bsets_profile0/p0_12_bset/ds0_ht_12_b11.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-ds0_ht_14_b11
fate-jpeg2000dec-ds0_ht_14_b11: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/htj2k_bsets_profile0/p0_14_bset/ds0_ht_14_b11.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-ds0_hm_15_b8
fate-jpeg2000dec-ds0_hm_15_b8: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/htj2k_bsets_profile0/p0_15_bset/ds0_hm_15_b8.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-ds0_ht_15_b11
fate-jpeg2000dec-ds0_ht_15_b11: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/htj2k_bsets_profile0/p0_15_bset/ds0_ht_15_b11.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-ds0_ht_15_b14
fate-jpeg2000dec-ds0_ht_15_b14: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/htj2k_bsets_profile0/p0_15_bset/ds0_ht_15_b14.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-ds0_ht_16_b11
fate-jpeg2000dec-ds0_ht_16_b11: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/htj2k_bsets_profile0/p0_16_bset/ds0_ht_16_b11.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-ds1_ht_01_b11
fate-jpeg2000dec-ds1_ht_01_b11: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/htj2k_bsets_profile1/p1_01_bset/ds1_ht_01_b11.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-ds1_ht_01_b12
fate-jpeg2000dec-ds1_ht_01_b12: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/htj2k_bsets_profile1/p1_01_bset/ds1_ht_01_b12.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-ds1_ht_02_b11
fate-jpeg2000dec-ds1_ht_02_b11: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/htj2k_bsets_profile1/p1_02_bset/ds1_ht_02_b11.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-ds1_ht_02_b12
fate-jpeg2000dec-ds1_ht_02_b12: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/htj2k_bsets_profile1/p1_02_bset/ds1_ht_02_b12.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-ds1_ht_03_b11
fate-jpeg2000dec-ds1_ht_03_b11: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/htj2k_bsets_profile1/p1_03_bset/ds1_ht_03_b11.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-ds1_ht_03_b12
fate-jpeg2000dec-ds1_ht_03_b12: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/htj2k_bsets_profile1/p1_03_bset/ds1_ht_03_b12.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-ds1_ht_04_b9
fate-jpeg2000dec-ds1_ht_04_b9: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/htj2k_bsets_profile1/p1_04_bset/ds1_ht_04_b9.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-ds1_ht_05_b11
fate-jpeg2000dec-ds1_ht_05_b11: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/htj2k_bsets_profile1/p1_05_bset/ds1_ht_05_b11.j2k

FATE_JPEG2000DEC += fate-jpeg2000dec-ds1_ht_06_b11
fate-jpeg2000dec-ds1_ht_06_b11: CMD = framecrc -flags +bitexact -i $(TARGET_SAMPLES)/jpeg2000/itu-iso/htj2k_bsets_profile1/p1_06_bset/ds1_ht_06_b11.j2k

FATE_JPEG2000DEC += $(FATE_JPEG2000DEC-yes)

FATE_SAMPLES_FFMPEG-$(call FRAMECRC, IMAGE_J2K_PIPE, JPEG2000) += $(FATE_JPEG2000DEC)
fate-jpeg2000dec: $(FATE_JPEG2000DEC)
