/*
 * @creation 1998-08-19
 * @modification $Date: 2006-09-28 13:39:11 $
 * @license GNU General Public License 2
 * @copyright (c)1998-2001 CETMEF 2 bd Gambetta F-60231 Compiegne
 * @mail devel@fudaa.org
 */
package org.kalypso.kalypsomodel1d2d.conv.telemac;

import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;

/**
 * Une classe etendant DataOutputStream et permettant de facilement ecrire des fichiers binaires. dependants de la
 * machine (sparc, i386, ...)
 * 
 * @version $Revision: 1.5 $ $Date: 2006-09-28 13:39:11 $ by $Author: deniger $
 * @author Axel von Arnim
 */
public class NativeBinaryOutputStream extends DataOutputStream {

  NativeBinarySystem syst_ = new NativeBinarySystem();
  byte[] buf_;

  /**
   * @param _out le flux suppor
   * @param _machine la description ou l'id de la machine
   * @see NativeBinarySystem#getLocalMachine()
   */
  public NativeBinaryOutputStream(final OutputStream _out, final String _machine) {
    super(_out);
    buf_ = null;
    setMachineType(_machine);
  }

  /**
   * @param _machine le type de la machine (X86 ou sparc)
   * @see NativeBinarySystem#getLocalMachine()
   */
  public final void setMachineType(final String _machine) {
    syst_.setMachineType(_machine);
  }

  /**
   * @return le type de machine utilise
   */
  public String getMachineType() {
    return syst_.getMachineType();
  }

  /**
   * @param _int8 l'entier a ecrire sur 1 byte
   * @throws IOException
   */
  public void writeInt8(final byte _int8) throws IOException {
    buf_ = new byte[1];
    buf_[0] = _int8;
    write(buf_);
  }

  /**
   * @param _uint8 l'entier non signe a ecrire sur 1 byte
   * @throws IOException
   */
  public void writeUInt8(final short _uint8) throws IOException {
    buf_ = new byte[1];
    buf_[0] = _uint8 > 127 ? (byte) (_uint8 - 256) : (byte) _uint8;
    write(buf_);
  }

  /**
   * @param _int16 l'entier a ecrire sur 2 byte
   * @throws IOException
   */
  public void writeInt16(final short _int16) throws IOException {
    int tmp;
    int signCorr;
    if (_int16 < 0) {
      signCorr = 1;
    } else {
      signCorr = 0;
    }
    buf_ = new byte[2];
    tmp = _int16 % 0x100;
    buf_[syst_.s1_] = (byte) (tmp);
    tmp = _int16 / 0x100;
    buf_[syst_.s2_] = (byte) (tmp - signCorr);
    write(buf_);
  }

  /**
   * @param _uint16 l'entier non signe a ecrire sur 2 octets
   * @throws IOException
   */
  public void writeUInt16(final int _uint16) throws IOException {
    int tmp;
    buf_ = new byte[2];
    tmp = _uint16 % 0x100;
    buf_[syst_.s1_] = (byte) (tmp > 127 ? tmp - 256 : tmp);
    tmp = _uint16 / 0x100;
    buf_[syst_.s2_] = (byte) (tmp); // (byte)(tmp>127?tmp-256:tmp);
    write(buf_);
  }

  /**
   * @param _int32 l'entier a ecrire sur 4 octets
   * @throws IOException
   */
  public int writeInt32(final int _int32) throws IOException {
    int tmp;
    int signCorr;
    if (_int32 < 0) {
      signCorr = 1;
    } else {
      signCorr = 0;
    }
    buf_ = new byte[4];
    tmp = _int32 / 0x1000000;
    buf_[syst_.i4_] = (byte) (tmp - signCorr); // (byte)(tmp>127?tmp-25:tmp);
    tmp = _int32 / 0x10000;
    buf_[syst_.i3_] = (byte) (tmp - signCorr); // (byte)(tmp>127?tmp-25:tmp);
    tmp = _int32 / 0x100;
    buf_[syst_.i2_] = (byte) (tmp - signCorr); // (byte)(tmp>127?tmp-25:tmp);
    buf_[syst_.i1_] = (byte) (_int32 % 0x100);
    write(buf_);
    return buf_.length;
  }

  /**
   * @param _uint32 l'entier non signe a ecrire sur 4 octets
   * @throws IOException
   */
  public void writeUInt32(final long _uint32) throws IOException {
    long tmp;
    buf_ = new byte[4];
    tmp = _uint32 / 0x1000000L;
    buf_[syst_.i4_] = (byte) (tmp); // (byte)(tmp>127?tmp-256:tmp);
    tmp = _uint32 / 0x10000L;
    buf_[syst_.i3_] = (byte) (tmp); // (byte)(tmp>127?tmp-256:tmp);
    tmp = _uint32 / 0x100L;
    buf_[syst_.i2_] = (byte) (tmp); // (byte)(tmp>127?tmp-256:tmp);
    tmp = _uint32 % 0x100L;
    buf_[syst_.i1_] = (byte) (tmp > 127 ? tmp - 256 : tmp);
    write(buf_);
  }

  /**
   * @param _int64 l'entier a ecrire sur 8 octets
   * @throws IOException
   */
  public int writeInt64(final long _int64) throws IOException {
    long tmp;
    long signCorr;
    if (_int64 < 0) {
      signCorr = 1;
    } else {
      signCorr = 0;
    }
    buf_ = new byte[8];
    tmp = _int64 / 0x100000000000000L;
    buf_[syst_.l8_] = (byte) (tmp - signCorr); // (byte)(tmp>127?tmp-256:tmp);
    tmp = _int64 / 0x1000000000000L;
    buf_[syst_.l7_] = (byte) (tmp - signCorr); // (byte)(tmp>127?tmp-256:tmp);
    tmp = _int64 / 0x10000000000L;
    buf_[syst_.l6_] = (byte) (tmp - signCorr); // (byte)(tmp>127?tmp-256:tmp);
    tmp = _int64 / 0x100000000L;
    buf_[syst_.l5_] = (byte) (tmp - signCorr); // (byte)(tmp>127?tmp-256:tmp);
    tmp = _int64 / 0x1000000L;
    buf_[syst_.l4_] = (byte) (tmp - signCorr); // (byte)(tmp>127?tmp-256:tmp);
    tmp = _int64 / 0x10000L;
    buf_[syst_.l3_] = (byte) (tmp - signCorr); // (byte)(tmp>127?tmp-256:tmp);
    tmp = _int64 / 0x100L;
    buf_[syst_.l2_] = (byte) (tmp - signCorr); // (byte)(tmp>127?tmp-256:tmp);
    buf_[syst_.l1_] = (byte) (_int64 % 0x100L);
    write(buf_);
    return buf_.length;
  }

  /**
   * @param _float32 le float a ecrire sur 4 octets
   * @throws IOException
   */
  public int writeFloat32(final float _float32) throws IOException {
    // B.M. Java autorise la valeur -0.0 qui perturbe le codage en bytes.
    // L'intérêt de cette valeur étant très relative, on la remplace par 0.0
    float f = _float32;
    if (f == -0.0f) {
      f = 0.0f;
    }
    return writeInt32(Float.floatToIntBits(f));
  }

  /**
   * @param _float64 le float a ecrire sur 8 octets
   * @throws IOException
   */
  public int writeFloat64(final double _float64) throws IOException {
    // B.M. Java autorise la valeur -0.0 qui perturbe le codage en bytes.
    // L'intérêt de cette valeur étant très relative, on la remplace par 0.0
    double f = _float64;
    if (f == -0.0) {
      f = 0.0;
    }
    return writeInt64(Double.doubleToLongBits(f));
  }
}