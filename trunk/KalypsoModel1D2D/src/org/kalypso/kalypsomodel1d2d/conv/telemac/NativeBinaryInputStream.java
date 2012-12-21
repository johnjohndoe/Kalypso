/*
 * @creation 1998-08-19
 * @modification $Date: 2006-09-28 13:39:11 $
 * @license GNU General Public License 2
 * @copyright (c)1998-2001 CETMEF 2 bd Gambetta F-60231 Compiegne
 * @mail devel@fudaa.fr
 */
package org.kalypso.kalypsomodel1d2d.conv.telemac;

import java.io.BufferedInputStream;
import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * Une classe etendant DataInputStream et permettant de facilement lire des fichiers binaires dependants de la machine
 * (sparc, i386, ...).
 * 
 * @version $Id: NativeBinaryInputStream.java,v 1.6 2006-09-28 13:39:11 deniger Exp $
 * @author Axel von Arnim
 */
public class NativeBinaryInputStream// extends DataInputStream
{

  /**
   * entier renvoye par la methode read.
   */
  private int nbByteLus_;
  NativeBinarySystem system_ = new NativeBinarySystem();
  byte[] buf_;
  private DataInputStream in_;

  /**
   * L'inputStream est transforme : "habille" en BufferedInputStream.
   * 
   * @param _in le flux support
   * @param _bufferLength la taille du buffer
   * @param _machine l'identifiant de la machine
   * @throws IOException
   */
  public NativeBinaryInputStream(final InputStream _in, final int _bufferLength, final String _machine) {
    this(new BufferedInputStream(_in, _bufferLength), _machine);
  }

  /**
   * @param _in le flux support
   * @param _machine l'identifiant de la machine
   * @throws IOException
   */
  public NativeBinaryInputStream(final InputStream _in, final String _machine) {
    this(new BufferedInputStream(_in), _machine);
  }

  /**
   * @param _in le flux support
   * @param _machine l'identifiant de la machine
   * @throws IOException
   */
  public NativeBinaryInputStream(final BufferedInputStream _in, final String _machine) {
    in_ = new DataInputStream(_in);
    setMachineType(_machine);
    buf_ = new byte[8];
  }

  /**
   * Lecture d'un champ chaine de caractères " <I>character </I>" Fortran.
   * 
   * @param _lgString Longueur de la chaine à lire
   * @return String correspondant à la chaine de caractères
   * @throws IOException
   */
  public String readCharacter(final int _lgString) throws IOException {
    if (buf_.length != _lgString) {
      buf_ = new byte[_lgString];
    }
    in_.read(buf_);
    return new String(buf_);
  }

  /**
   * Cette methode assure (au maximum) que _l octets sont ignores.
   * 
   * @return le nombre d'octets reellement ignore.
   * @param _l le nombre d'octets a ignorer
   * @throws IOException
   */
  public long skip(final long _l) throws IOException {
    final long skipReal = in_.skip(_l);
    // si tous les octets n'ont pas ete ignore
    if (skipReal < _l) {
      // les octets restants a lire.
      final long nToSkip = _l - skipReal;
      // le nombre total d'octets ignores
      long total = 0L;
      // les octets ignores lors d'une instruction.
      long nSkipped = 0L;
      // tant qu'on pas atteint le but et qu'il reste des octets a lire.
      while ((total < nToSkip) && ((nSkipped = in_.skip(nToSkip - total)) > 0)) {
        total += nSkipped;
      }
      return _l - nToSkip + total;
    }
    return skipReal;
  }

  /**
   * @throws IOException
   */
  public void close() throws IOException {
    in_.close();
  }

  /**
   * @return le nb d'octet restant
   * @throws IOException
   */
  public int available() throws IOException {
    return in_.available();
  }

  /**
   * @param _l le nombre de Bytes a ignoree
   * @throws IOException
   * @see DataInputStream#skipBytes(int)
   */
  public void skipBytes(final int _l) throws IOException {
    in_.skipBytes(_l);
  }

  /**
   * Affecte le type de machine. Par défaut, le type sparc est utilise.
   * 
   * @param _machine le type de la machine
   */
  public final void setMachineType(final String _machine) {
    system_.setMachineType(_machine);
  }

  /**
   * @return l'identifiant de la machine utilisee
   */
  public String getMachineType() {
    return system_.getMachineType();
  }

  private long internReadInt64() throws IOException {
    if (buf_.length != 8) {
      buf_ = new byte[8];
    }
    nbByteLus_ = in_.read(buf_);
    return (buf_[system_.l1_] < 0 ? (256 + buf_[system_.l1_]) : buf_[system_.l1_])
        + (buf_[system_.l2_] < 0 ? (256 + buf_[system_.l2_]) : buf_[system_.l2_]) * 0x100L
        + (buf_[system_.l3_] < 0 ? (256 + buf_[system_.l3_]) : buf_[system_.l3_]) * 0x10000L
        + (buf_[system_.l4_] < 0 ? (256 + buf_[system_.l4_]) : buf_[system_.l4_]) * 0x1000000L
        + (buf_[system_.l5_] < 0 ? (256 + buf_[system_.l5_]) : buf_[system_.l5_]) * 0x100000000L
        + (buf_[system_.l6_] < 0 ? (256 + buf_[system_.l6_]) : buf_[system_.l6_]) * 0x10000000000L
        + (buf_[system_.l7_] < 0 ? (256 + buf_[system_.l7_]) : buf_[system_.l7_]) * 0x1000000000000L
        + (buf_[system_.l8_]) * 0x100000000000000L;
  }

  /**
   * @return le bit lu
   * @throws IOException
   */
  public byte readInt8() throws IOException {
    if (buf_.length != 1) {
      buf_ = new byte[1];
    }
    nbByteLus_ = in_.read(buf_);
    return buf_[0];
  }

  /**
   * @return les bit lu non signe
   * @throws IOException
   */
  public short readUInt8() throws IOException {
    if (buf_.length != 1) {
      buf_ = new byte[1];
    }
    nbByteLus_ = in_.read(buf_);
    return buf_[0] < 0 ? (short) (256 + buf_[0]) : (short) buf_[0];
  }

  /**
   * Indique si la fin du fichier est atteinte. Si <code>true</code> ,cela signifie que la derniere lecture est
   * erronnee.
   * 
   * @return si fin du fichier.
   */
  public boolean isFinFichier() {
    return nbByteLus_ == -1;
  }

  /**
   * @return le short lu (2 byte)
   * @throws IOException
   */
  public short readInt16() throws IOException {
    if (buf_.length != 2) {
      buf_ = new byte[2];
    }
    nbByteLus_ = in_.read(buf_);
    return (short) ((buf_[system_.s1_] < 0 ? (256 + buf_[system_.s1_]) : buf_[system_.s1_]) + (buf_[system_.s2_] * 0x100));
  }

  /**
   * @return le int lu (2 byte) non signe
   * @throws IOException
   */
  public int readUInt16() throws IOException {
    if (buf_.length != 2) {
      buf_ = new byte[2];
    }
    nbByteLus_ = in_.read(buf_);
    return (buf_[system_.s1_] < 0 ? (256 + buf_[system_.s1_]) : buf_[system_.s1_])
        + (buf_[system_.s2_] < 0 ? (256 + buf_[system_.s2_]) : buf_[system_.s2_]) * 0x100;
  }

  /**
   * @return le int lu (4 byte)
   * @throws IOException
   */
  public int readInt32() throws IOException {
    return internReadInt32();
  }

  private int internReadInt32() throws IOException {
    if (buf_.length != 4) {
      buf_ = new byte[4];
    }
    nbByteLus_ = in_.read(buf_);
    return (buf_[system_.i1_] < 0 ? (256 + buf_[system_.i1_]) : buf_[system_.i1_])
        + (buf_[system_.i2_] < 0 ? (256 + buf_[system_.i2_]) : buf_[system_.i2_]) * 0x100
        + (buf_[system_.i3_] < 0 ? (256 + buf_[system_.i3_]) : buf_[system_.i3_]) * 0x10000
        + (buf_[system_.i4_] * 0x1000000);
  }

  /**
   * @return le int lu (4 byte) non signe
   * @throws IOException
   */
  public long readUInt32() throws IOException {
    if (buf_.length != 4) {
      buf_ = new byte[4];
    }
    nbByteLus_ = in_.read(buf_);
    return (buf_[system_.i1_] < 0 ? (256 + buf_[system_.i1_]) : buf_[system_.i1_])
        + (buf_[system_.i2_] < 0 ? (256 + buf_[system_.i2_]) : buf_[system_.i2_]) * 0x100L
        + (buf_[system_.i3_] < 0 ? (256 + buf_[system_.i3_]) : buf_[system_.i3_]) * 0x10000L
        + (buf_[system_.i4_] < 0 ? (256 + buf_[system_.i4_]) : buf_[system_.i4_]) * 0x1000000L;
  }

  /**
   * Utilise une methode privee pour eviter les problemes avec l'heritage.
   * 
   * @return le int lu ( 8 byte)
   * @throws IOException
   */
  public long readInt64() throws IOException {

    return internReadInt64();
  }

  /**
   * Reviens a faire readInt_32(). Ne pas utiliser cette méthode car fausse l'incrementation de
   * FortranBinaryInputStream.
   * 
   * @return le float lu ( 4 byte)
   * @throws IOException
   */
  public float readFloat32() throws IOException {
    final int temp = internReadInt32();
    return Float.intBitsToFloat(temp);
  }

  /**
   * Reviens a faire readInt_64(). Ne pas utiliser cette méthode car fausse l'incrementation de FortranBinaryInputStream
   * 
   * @return le double lu (8 byte)
   * @throws IOException
   */
  public double readFloat64() throws IOException {

    final long temp = internReadInt64();
    return Double.longBitsToDouble(temp);
  }
}