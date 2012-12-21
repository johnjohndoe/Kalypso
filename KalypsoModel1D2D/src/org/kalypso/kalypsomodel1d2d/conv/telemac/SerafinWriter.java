/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
/*
 * 
 * @creation 2002-11-20
 * 
 * @modification $Date: 2007-05-04 13:47:27 $
 * 
 * @license GNU General Public License 2
 * 
 * @copyright (c)1998-2001 CETMEF 2 bd Gambetta F-60231 Compiegne
 * 
 * @mail devel@fudaa.org
 */
package org.kalypso.kalypsomodel1d2d.conv.telemac;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.kalypso.kalypsomodel1d2d.conv.results.RestartNodes;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * this implementation based on SerafinWriter.java from "fudaa-prepro". (@http://prepro.fudaa.fr/)
 * 
 * @author Fred Deniger, ig
 * 
 */
public class SerafinWriter
{
  enum coordType
  {
    X,
    Y,
    Z;
  }

  public static final int IPARAM_NB = 10;

  private String m_machineID;

  OutputStream m_out;

  private int[] m_iparams;

  private List<GM_Triangle> m_triangles;

  private List<GM_Position> m_listNodes;

  private String m_crs;

  private int[] m_intParamsOrder = { 0, 1, 2, 3 };

  private String m_projectName;

  private boolean m_precisionDouble = false;

  private List<GM_Position> m_listBoundNodes;

  private double m_initWL = 0.;

  public static String m_precisionDoubleString = "SERAFIND"; //$NON-NLS-1$

  public static String m_precisionSingleString = "SERAFIN"; //$NON-NLS-1$

  public SerafinWriter( final String projectName, final List<GM_Triangle> triangles, final List<GM_Position> listNodes, final List<GM_Position> listBoundNodes, final int[] iparams, final String crs )
  {
    // setMachineX86();
    setMachineSPARC();
    // set according to fudaa as standard to SPARC
    // m_machineID = System.getProperty("os.arch");
    m_projectName = projectName;
    m_triangles = triangles;
    m_listNodes = listNodes;
    
    m_listBoundNodes = listBoundNodes;
    m_iparams = iparams;
    m_crs = crs;

  }

  public boolean isPrecisionDouble( )
  {
    return m_precisionDouble;
  }

  public void setPrecisionDouble( boolean precisionDouble )
  {
    m_precisionDouble = precisionDouble;
  }

  private void setOut( final OutputStream _out )
  {
    m_out = _out;
  }

  public void setFile( final File _f )
  {
    FileOutputStream out = null;
    try
    {
      out = new FileOutputStream( _f );
    }
    catch( final FileNotFoundException _e )
    {
    }
    if( out != null )
    {
      setOut( out );
    }
  }

  public void setMachineID( final String id )
  {
    m_machineID = NativeBinarySystem.getMachineId( id );
  }

  /**
   * Initialise le type de machine pour sparc.
   */
  public void setMachineSPARC( )
  {
    m_machineID = NativeBinarySystem.SPARC;
  }

  /**
   * Initialise le type de machine pour X 86.
   */
  public void setMachineX86( )
  {
    m_machineID = NativeBinarySystem.X86;
  }

  public void writeAll( final String[] paramNames, final String[] paramUnits, final RestartNodes restartNodes/*, final int timeSteps*/ ) throws IOException
  {

    final FortranBinaryOutputStream out = new FortranBinaryOutputStream( m_out, true, m_machineID );
    out.setDoublePrecision( m_precisionDouble );

    final int nbv1 = paramNames.length;
    final int nelem = m_triangles.size();
    final int npt = m_listNodes.size();
    final int nppel = 3; // FIXME: make variable if meaning full //maillage.getElement(0).getPtNb();

    final String header = adjustSize( 72, m_projectName );
    final String headerPrecision = adjustSize( 8, (m_precisionDouble ? m_precisionDoubleString : m_precisionSingleString) ); //$NON-NLS-1$
    out.writeCharacter( header );
    out.writeCharacter( headerPrecision );
    out.writeRecord();
    // NBV1
    out.writeInteger( nbv1 );
    // NBV2
    out.writeInteger( 0 );
    out.writeRecord();
    // TEXTi LUNITi
    writeVariables( paramNames, paramUnits, out, nbv1 );

    // IPARAM
    writeIparam( out, m_iparams );

    out.writeInteger( nelem );
    out.writeInteger( npt );
    out.writeInteger( nppel );

    out.writeInteger( 1 );
    out.writeRecord();

    Map< GM_Position, Integer > mapNodes = new HashMap<>();
    int posIndex = 1;
    for( GM_Position pos : m_listNodes )
    {
      mapNodes.put( pos, posIndex++ );
    }
    for( int i = 0; i < nelem; i++ )
    {
      GM_Triangle tri = m_triangles.get( i );
      GM_Position[] poses = tri.getExteriorRing();
      for( int j = 0; j < nppel; j++ )
      {
//        int iNode = m_listNodes.indexOf( poses[ j ] ) + 1;
        int iNode = mapNodes.get( poses[ j ] );
        out.writeInteger( iNode );// lieltIndex.getPtIndex(j) + 1);
      }
    }
    out.writeRecord();

    writeIpobo( m_listNodes, out );

    writeNodesCoordInfo( out, coordType.X );

    writeNodesCoordInfo( out, coordType.Y );

    // timestep
    out.writeReal( 0.f );
    // out.writeFloatingPointValue( 0.f );
    out.writeRecord();

    writeNodesCoordInfo( out, coordType.Z );
    if( restartNodes != null )
    {
      writeRestartParams( out, restartNodes );
    }

    out.flush();
    out.close();
  }

  private void writeRestartParams( final FortranBinaryOutputStream out, final RestartNodes restartNodes ) throws IOException
  {
    for( int t = 0; t < m_intParamsOrder.length; t++ )
    {
      for( int i = 0; i < m_listNodes.size(); i++ )
      {
        GM_Position actPos = m_listNodes.get( i );
        INodeResult restartNode = restartNodes.getNodeResultAtPosition( GeometryFactory.createGM_Point( actPos, m_crs ) );
        switch( m_intParamsOrder[ t ] )
        {
          case 0:
            Double velValueU = 0.;
            try
            {
              velValueU = restartNode.getVelocity().get( 0 );
            }
            catch( Exception e )
            {
              // TODO: handle exception 
            }
            out.writeFloatingPointValue( velValueU );
            break;
          case 1:
            Double velValueV = 0.;
            try
            {
              velValueV = restartNode.getVelocity().get( 1 );
            }
            catch( Exception e )
            {
              // TODO: handle exception
            }
            out.writeFloatingPointValue( velValueV );
            break;
          case 2:
            double depthValue = 0.;
            try
            {
              depthValue = restartNode.getDepth();
            }
            catch( Exception e )
            {
              // TODO: handle exception
            }
            out.writeFloatingPointValue( depthValue );
            break;
          case 3:
            double waterlevelValue = m_initWL;
            try
            {
              waterlevelValue = restartNode.getWaterlevel();
            }
            catch( Exception e )
            {
              // TODO: handle exception
            }
            out.writeFloatingPointValue( waterlevelValue );
            break;

          default:
            // error bad it is
            break;
        }
      }
      out.writeRecord();
    }
  }

  private void writeNodesCoordInfo( final FortranBinaryOutputStream out, final coordType type ) throws IOException
  {
    for( GM_Position pos : m_listNodes )
    {
      switch( type )
      {
        case X:
          // out.writeFloatingPointValue( pos.getX() );
          if( !out.isDoubleToFloatError( pos.getX() ) )
            out.writeReal( (float) (pos.getX() - m_iparams[2]) );
          else
            throw new IOException( "invalid coordinate X" );
          break;
        case Y:
          // out.writeFloatingPointValue( pos.getY() );
          if( !out.isDoubleToFloatError( pos.getY() ) )
            out.writeReal( (float) (pos.getY() - m_iparams[3]) );
          else
            throw new IOException( "invalid coordinate Y" );

          break;
        case Z:
          if( !out.isDoubleToFloatError( pos.getZ() ) )
            out.writeReal( (float) (pos.getZ()) );
          else
            throw new IOException( "invalid coordinate Y" );
          // out.writeFloatingPointValue( pos.getZ() );
          break;

        default:
          // should not be possible
          break;
      }
    }
    out.writeRecord();

  }

  private void writeIpobo( final List<GM_Position> nodes, final FortranBinaryOutputStream out ) throws IOException
  {
    for( GM_Position pos : nodes )
    {
      Integer boundCond = m_listBoundNodes.indexOf( pos ) + 1;
      out.writeInteger( boundCond );

    }
    out.writeRecord();
  }

  /*
  private void writeDate( final long date, final FortranBinaryOutputStream out ) throws IOException
  {
    final Calendar c = Calendar.getInstance();
    c.setTime( new Date( date ) );
    out.writeInteger( c.get( Calendar.YEAR ) );
    out.writeInteger( c.get( Calendar.MONTH ) );
    out.writeInteger( c.get( Calendar.DAY_OF_MONTH ) );
    out.writeInteger( c.get( Calendar.HOUR_OF_DAY ) );
    out.writeInteger( c.get( Calendar.MINUTE ) );
    out.writeInteger( c.get( Calendar.SECOND ) );
    out.writeRecord();
  }
  */

  private void writeIparam( final FortranBinaryOutputStream _out, final int[] _iparam ) throws IOException
  {
    final int tI = _iparam.length;
    for( int i = 0; i < tI; i++ )
    {
      _out.writeInteger( _iparam[i] );
    }
    _out.writeRecord();
  }

  private void writeVariables( final String[] names, final String[] units, final FortranBinaryOutputStream out, final int nbv1 ) throws IOException
  {
    String tS;
    for( int i = 0; i < nbv1; i++ )
    {
      tS = adjustSize( 16, names[i] );
      out.writeCharacter( tS );
      tS = adjustSize( 16, units[i] );
      out.writeCharacter( tS );
      out.writeRecord();
    }
  }

  private String adjustSize( final int i, final String s )
  {
    return adjustSize( i, s, false );
  }

  /**
   * Ajuste la taille de _s a <code>_taille</code>. Si la taille de _s est plus grande, les derniers caracteres de s
   * seront enleves. Sinon, les espaces nÈcessaires seront ajoutes.
   * 
   * @param _taille
   *          la taille
   * @param _s
   *          la chaine a ajuster
   * @param _addSpaceBefore
   *          true si les espace doivent etre ajoute avant
   * @return la chaine ajustee.
   */
  private String adjustSize( final int _taille, final String _s, boolean _addSpaceBefore )
  {
    final String s = _s == null ? "" : _s;
    final int l = s.length();
    if( l > _taille )
    {
      return s.substring( 0, _taille );
    }
    final StringBuffer r = new StringBuffer( _taille );
    if( !_addSpaceBefore )
    {
      r.append( s );
    }
    final int lMotif = 5;
    final String b = "     ";
    final int nbMotif = (_taille - l) / lMotif;
    final int sup = (_taille - l) % lMotif;
    for( int i = nbMotif; i > 0; i-- )
    {
      r.append( b );
    }
    switch( sup )
    {
      case 0:
        break;
      case 1:
        r.append( " " );
        break;
      case 2:
        r.append( "  " );
        break;
      case 3:
        r.append( "   " );
        break;
      case 4:
        r.append( "    " );
        break;
      default:
        break;
    }
    if( _addSpaceBefore )
    {
      r.append( s );
    }
    if( r.length() != _taille )
    {
      //
    }
    return r.toString();
  }
}