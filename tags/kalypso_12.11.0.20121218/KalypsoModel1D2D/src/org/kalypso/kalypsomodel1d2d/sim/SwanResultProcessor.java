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
package org.kalypso.kalypsomodel1d2d.sim;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.URISyntaxException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Formatter;
import java.util.List;
import java.util.Locale;
import java.util.StringTokenizer;

import org.apache.commons.vfs2.FileObject;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.kalypsomodel1d2d.conv.SWANDataConverterHelper;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypsodeegree.model.geometry.GM_Position;

/**
 * @author Gernot Belger
 * @author Ilya Gershovich
 */
public class SwanResultProcessor
{
  private FileObject m_swanResultDir;

  private final IControlModel1D2D m_controlModel;

  private final File m_outputDir;

  public SwanResultProcessor( final FileObject swanResultDIR, final IControlModel1D2D controlModel, final File outputDir )
  {
    m_swanResultDir = swanResultDIR;
    m_controlModel = controlModel;
    m_outputDir = outputDir;
  }

  public IStatus execute( )
  {
    try
    {
      if( m_swanResultDir.getName().getBaseName().endsWith( "zip" ) ) //$NON-NLS-1$
      {
        // swan mat file should be unpacked for using in within JMatIO-Reader, so we put the uncompressed version in
        // to the working directory.
        ZipUtilities.unzip( new File( m_swanResultDir.getURL().toURI() ), new File( m_outputDir.toURI() ) );
        // FIXME: why is the result dir changed here?

        // TODO: hm, maybe this was intended to process the unpacked result, so we should not return?
        m_swanResultDir = VFSUtilities.getNewManager().resolveFile( m_outputDir.toURI().toURL().toExternalForm() );
        return Status.OK_STATUS;
      }

      final FileObject swanResFile = m_swanResultDir.getChild( ISimulation1D2DConstants.SIM_SWAN_TRIANGLE_FILE + "." + ISimulation1D2DConstants.SIM_SWAN_MAT_RESULT_EXT ); //$NON-NLS-1$
      final FileObject swanResShiftFile = m_swanResultDir.getChild( ISimulation1D2DConstants.SIM_SWAN_COORD_SHIFT_FILE );
      final FileObject swanResOutTabFile = m_swanResultDir.getChild( ISimulation1D2DConstants.SIM_SWAN_TRIANGLE_FILE + "_out.tab" ); //$NON-NLS-1$
      processSWANTabFile( swanResOutTabFile, swanResShiftFile );
      final File zipOutput = new File( m_outputDir, ISimulation1D2DConstants.SIM_SWAN_TRIANGLE_FILE + ".zip" ); //$NON-NLS-1$
      final List<File> lListFilesToZip = new ArrayList<>();
      lListFilesToZip.add( new File( swanResFile.getURL().toURI() ) );
      lListFilesToZip.add( new File( swanResShiftFile.getURL().toURI() ) );
      lListFilesToZip.add( new File( swanResOutTabFile.getURL().toURI() ) );
      if( m_controlModel.getINITialValuesSWAN() == 3 )
      {
        final FileObject swanResHotFile = m_swanResultDir.getChild( ISimulation1D2DConstants.SIM_SWAN_HOT_FILE );
        lListFilesToZip.add( new File( swanResHotFile.getURL().toURI() ) );
      }
      ZipUtilities.zip( zipOutput, lListFilesToZip.toArray( new File[lListFilesToZip.size()] ), new File( m_swanResultDir.getURL().toURI() ) );

      return Status.OK_STATUS;
    }
    catch( final IOException e )
    {
      // FIXME: This is not real error handling!
      e.printStackTrace();
      return StatusUtilities.statusFromThrowable( e );
    }
    catch( final URISyntaxException e )
    {
      // FIXME: This is not real error handling!
      // TODO Auto-generated catch block
      e.printStackTrace();
      return StatusUtilities.statusFromThrowable( e );
    }
  }

  // FIXME: move into separate class
  private void processSWANTabFile( final FileObject swanResOutTabFile, final FileObject swanResShiftFile )
  {
    final GM_Position lShiftPosition = SWANDataConverterHelper.readCoordinateShiftValues( swanResShiftFile );
    if( lShiftPosition == null )
      return;

    try
    {
      // FIXME: why?! should never happen...!
      if( swanResOutTabFile.isContentOpen() )
        swanResOutTabFile.close();

      final FileObject swanResOutTabFileBackUp = swanResOutTabFile.getParent().resolveFile( swanResOutTabFile.getName().getBaseName() + ".bck" ); //$NON-NLS-1$
      swanResOutTabFile.moveTo( swanResOutTabFileBackUp );

      final OutputStream lOutStream = swanResOutTabFile.getContent().getOutputStream();
      final Formatter lFormatter = new Formatter( lOutStream, Charset.defaultCharset().name(), Locale.US );

      final BufferedReader lInDataStream = new BufferedReader( new InputStreamReader( swanResOutTabFileBackUp.getContent().getInputStream() ) );

      while( lInDataStream.ready() )
      {
        final String lStrTmpLine = lInDataStream.readLine().trim();
        if( lStrTmpLine.startsWith( "%" ) ) //$NON-NLS-1$
        {
          lFormatter.format( "%s\n", lStrTmpLine ); //$NON-NLS-1$
          continue;
        }

        final StringTokenizer lStrTokenizer = new StringTokenizer( lStrTmpLine, " " ); //$NON-NLS-1$
        int lIntTokenCounter = 0;
        String lStrNewLine = ""; //$NON-NLS-1$
        while( lStrTokenizer.hasMoreTokens() )
        {
          final String lStrToken = lStrTokenizer.nextToken();
          if( lIntTokenCounter == 1 )
          {
            lStrNewLine += String.format( Locale.US, "%.5f\t", NumberUtils.parseQuietDouble( lStrToken ) + lShiftPosition.getX() ); //$NON-NLS-1$
          }
          else if( lIntTokenCounter == 2 )
          {
            lStrNewLine += String.format( Locale.US, "%.5f\t", NumberUtils.parseQuietDouble( lStrToken ) + lShiftPosition.getY() ); //$NON-NLS-1$
          }
          else
          {
            lStrNewLine += lStrToken + "\t"; //$NON-NLS-1$
          }
          lIntTokenCounter++;
        }
        lFormatter.format( "%s\n", lStrNewLine ); //$NON-NLS-1$
      }

      // FIXME: not closed in a save way!
      lFormatter.close();
      lInDataStream.close();
      lOutStream.close();
    }
    catch( final Exception e )
    {
      // FIXME: this is no way to handle an error !
    }
  }

  public FileObject getSwanResultDir( )
  {
    return m_swanResultDir;
  }
}