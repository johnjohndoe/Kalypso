/**
 * @creation     1998-08-19
 * @modification $Date: 2006-09-19 14:42:29 $
 * @license      GNU General Public License 2
 * @copyright    (c)1998-2001 CETMEF 2 bd Gambetta F-60231 Compiegne
 * @mail devel@fudaa.fr
 */
package org.kalypso.kalypsomodel1d2d.conv.telemac;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;

/**
 * Une classe facilitant l'écriture de fichiers binaires lus par Fortran. ATTENTION les methodes heritees et non
 * redéfinies dans cette donneront des résultats faux. L'équivalence d'intructions entre Java et Fortran se fera de la
 * manière suivante :<BR>
 * 
 * <PRE>
 * (en considérant i=integer/int, f=real/float, d=double precision/double et
 * s=character*()/String)
 * 1) Pour un fichier à acces séquentiel :
 *   Fortran
 *     open  (unit=10,file='fichier.bin',access='sequentiel',form='unformatted')
 *     write (unit=10)
 *     ...
 *     close (unit=10)
 *   Java
 *     FortranBinaryOutputStream out=
 *      new FortranBinaryOutputStream(new FileOutputStream("fichier.bin"),true);
 *     out.writeInteger(i);
 *     out.writeReal(f);
 *     out.writeDoublePrecision(d);
 *     out.writeCharacter(s);
 *     out.writeRecord();
 *     ...
 *     out.close();
 * 2) Pour un fichier à acces direct :
 *   Fortran
 *     open(unit=10,file='fichier.bin',access='direct',recl=30,form='unformatted')
 *     write (unit=10,rec=1)
 *     ...
 *     close (unit=10)
 *   Java
 *     FortranBinaryOutputStream out=
 *      new FortranBinaryOutputStream(new FileOutputStream("fichier.bin"),false);
 *     out.setRecordLength(30);
 *     out.writeInteger(i);
 *     out.writeReal(f);
 *     out.writeDoublePrecision(d);
 *     out.writeCharacter(s);
 *     out.writeRecord();
 *     ...
 *     out.close();
 * </PRE>
 * 
 * @version $Id: FortranBinaryOutputStream.java,v 1.14 2006-09-19 14:42:29 deniger Exp $
 * @author Bertrand Marchand
 */
public class FortranBinaryOutputStream extends NativeBinaryOutputStream
{
  private NativeBinaryOutputStream bufStream_;

  private ByteArrayOutputStream arrayStream_;

  private boolean sequential_;

  private int recordLength_;

  private boolean m_doublePrecision = true;

  /**
   * Création en précisant si le fichier binaire est à access séquentiel ou non.
   * 
   * @param _out
   *          OutputStream
   * @param _sequential
   *          <B>true</B> le fichier est binaire à accès <I>Sequential</I>. <B>false</B> le fichier est binaire à accès
   *          <I>Direct</I>
   */
  public FortranBinaryOutputStream( final OutputStream _out, final boolean _sequential )
  {
    this( _out, _sequential, System.getProperty( "os.arch" ) );
  }

  /**
   * Création en précisant si le fichier binaire est à access séquentiel ou non.
   * 
   * @param _out
   *          OutputStream
   * @param _sequential
   *          <B>true</B> le fichier est binaire à accès <I>Sequential</I>. <B>false</B> le fichier est binaire à accès
   *          <I>Direct</I>
   * @param _architectureID
   *          l'architecture demandee
   */
  public FortranBinaryOutputStream( final OutputStream _out, final boolean _sequential, final String _architectureID )
  {
    super( _out, _architectureID );
    arrayStream_ = new ByteArrayOutputStream();
    bufStream_ = new NativeBinaryOutputStream( arrayStream_, _architectureID );
    sequential_ = _sequential;
    recordLength_ = 0;
  }

  /**
   * Affectation de la longueur des enregistrements (pour les fichiers à accès <I>Direct</I>).
   * 
   * @param _length
   *          Longueur d'enregistrement en longworld (1 longworld=4 octets)
   */
  public void setRecordLength( final int _length )
  {
    recordLength_ = _length;
  }

  /**
   * Retourne la longueur des enregistrements (pour les fichiers à accès <I>Direct</I>.
   * 
   * @return Longueur d'enregistrement en longworld (1 longworld=4 octets)
   */
  public int getRecordLength( )
  {
    return recordLength_;
  }

  /**
   * Ecriture d'un champ chaine de caractères "<I>character</I>" Fortran.
   * 
   * @param _s
   *          string correspondant à la chaine de caractères
   * @throws IOException
   */
  public void writeCharacter( final String _s ) throws IOException
  {
    bufStream_.writeBytes( _s );
  }

  // public void writeCharacter(char[] _c) {
  // }
  // public void writeCharacter(char _c) {
  // }
  /**
   * Ecriture d'un champ entier "<I>integer</I>" Fortran.
   * 
   * @param _i
   *          int correspondant au integer
   * @throws IOException
   */
  public void writeInteger( final int _i ) throws IOException
  {
    bufStream_.writeInt32( _i );
  }

  /**
   * Ecriture d'un champ réel "<I>real</I>" Fortran.
   * 
   * @param _f
   *          float correspondant au real
   * @throws IOException
   */
  public void writeReal( final float _f ) throws IOException
  {
    bufStream_.writeFloat32( _f );
  }

  /**
   * Ecriture d'un champ réel en double précision "<I>double precision</I>". Fortran
   * 
   * @param _d
   *          double correspondant au double precision
   * @throws IOException
   */
  public void writeDoublePrecision( final double _d ) throws IOException
  {
    bufStream_.writeFloat64( _d );
  }

  /**
   * Ecriture des champs de l'enregistrement. L'enregistrement doit etre écrit pour que les champs soient également
   * écrits sur le fichiers. Un enregistrement correspond à une instruction WRITE du Fortran
   * 
   * @throws IOException
   */
  public void writeRecord( ) throws IOException
  {
    if( sequential_ )
    {
      final int t = arrayStream_.size();
      writeInt32( t );
      arrayStream_.writeTo( this );
      writeInt32( t );
    }
    else
    {
      final byte[] buffer = arrayStream_.toByteArray();
      write( buffer, 0, Math.min( buffer.length, 4 * recordLength_ ) );
      for( int i = buffer.length; i < 4 * recordLength_; i++ )
      {
        write( 0 );
      }
    }
    arrayStream_.reset();
  }

  /**
   * Fermeture du fichier.
   */
  @Override
  public void close( ) throws IOException
  {
    super.close();
    arrayStream_.close();
    bufStream_.close();
  }

  public void writeFloatingPointValue( final double value ) throws IOException
  {
    double tD = getCorrectDoubleValue( value );
    if( !m_doublePrecision )
    {
      if( isDoubleToFloatError( tD ) )
      {
        // analyze_.addError("Erreur de conversion lors de l'écriture des Données", t);
        throw new NumberFormatException( "Bad double number: " + value );
      }
      else
      {
        writeReal( (float) value );
      }
    }
    else
    {
      writeDoublePrecision( value );
    }
  }

  public void setDoublePrecision( boolean doublePrecision )
  {
    m_doublePrecision = doublePrecision;
  }

  public boolean isDoublePrecision( )
  {
    return m_doublePrecision;
  }

  protected double getCorrectDoubleValue( final double _init )
  {
    double min = 1E-15;
    return (_init > -min && _init < min) ? 0 : _init;
  }

  /**
   * Teste si le double n'appartient pas a la plage des float.
   * 
   * @param _d
   * @return true si le double n'appartient pas a la plage des float
   */
  public boolean isDoubleToFloatError( final double _d )
  {
    final double d = _d > 0 ? _d : -_d;
    if( d > Float.MAX_VALUE )
    {
      return true;
    }
    if( (d > 0) && (d < Float.MIN_VALUE) )
    {
      return true;
    }
    return false;
  }
}
