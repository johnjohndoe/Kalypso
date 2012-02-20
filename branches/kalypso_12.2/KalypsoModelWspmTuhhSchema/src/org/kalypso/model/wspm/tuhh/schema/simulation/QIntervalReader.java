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
package org.kalypso.model.wspm.tuhh.schema.simulation;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.math.BigDecimal;

import org.apache.commons.io.IOCase;
import org.apache.commons.io.filefilter.WildcardFileFilter;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.wspm.tuhh.schema.i18n.Messages;
import org.kalypso.model.wspm.tuhh.schema.schemata.IWspmTuhhQIntervallConstants;
import org.kalypso.simulation.core.util.LogHelper;

/**
 * @author Gernot Belger
 */
public class QIntervalReader
{
  private final String[] m_polynomComponents = new String[] { //
      IWspmTuhhQIntervallConstants.DICT_COMPONENT_WATERLEVEL, //
      IWspmTuhhQIntervallConstants.DICT_COMPONENT_DEPTH, //
      IWspmTuhhQIntervallConstants.DICT_COMPONENT_AREA, //
      IWspmTuhhQIntervallConstants.DICT_COMPONENT_RUNOFF, //
      IWspmTuhhQIntervallConstants.DICT_COMPONENT_ALPHA, //
      IWspmTuhhQIntervallConstants.DICT_COMPONENT_DELTA_AREA, //
      IWspmTuhhQIntervallConstants.DICT_COMPONENT_DELTA_RUNOFF, //
      IWspmTuhhQIntervallConstants.DICT_COMPONENT_DELTA_ALPHA //
  };

  private final File m_inputDir;

  private final LogHelper m_log;

  private final QIntervalIndex m_intervalIndex;

  public QIntervalReader( final File inputDir, final QIntervalIndex intervalIndex, final LogHelper log )
  {
    m_inputDir = inputDir;
    m_intervalIndex = intervalIndex;
    m_log = log;
  }

  public void execute( final IProgressMonitor monitor ) throws CoreException
  {
    try
    {
      readResults();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      m_log.log( e, Messages.getString("QIntervalReader.0") ); //$NON-NLS-1$
      ProgressUtilities.done( monitor );
    }
  }

  private void readResults( ) throws IOException
  {
    /* Read w-points first: PROFxxx.xxxx.txt files */
    final FilenameFilter filter = new WildcardFileFilter( "PROF*.txt", IOCase.INSENSITIVE ); //$NON-NLS-1$ 
    final File[] profFiles = m_inputDir.listFiles( filter );
    if( profFiles == null || profFiles.length == 0 )
    {
      m_log.finish( IStatus.ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeProcessor.17" ) ); //$NON-NLS-1$
      return;
    }

    read( profFiles );

    if( m_log.checkCanceled() )
      return;

    final PolynomeReader polynomeReader = new PolynomeReader( m_intervalIndex, m_log );
    polynomeReader.read( new File( m_inputDir, "Polynome.TXT" ) ); //$NON-NLS-1$

    final BuildingPolygonReader buildingReader = new BuildingPolygonReader( m_intervalIndex, m_log );
    buildingReader.read( new File( m_inputDir, PolynomeProcessor.WEIR_FILE_NAME ) );
    buildingReader.read( new File( m_inputDir, PolynomeProcessor.BRIDGE_FILE_NAME ) );

    if( m_log.checkCanceled() )
      return;
  }

  private void read( final File[] profFiles )
  {
    for( final File profFile : profFiles )
    {
      final String name = profFile.getName();
      if( name.length() < 9 )
        continue;

      try
      {
        final String stationString = name.substring( 4, name.length() - 4 );
        final BigDecimal station = new BigDecimal( stationString );

        final QRelationFileReader fileReader = new QRelationFileReader( m_log, null, m_polynomComponents, m_intervalIndex );
        fileReader.setStation( station );
        fileReader.read( profFile );
      }
      catch( final Exception e )
      {
        m_log.log( e, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeProcessor.20" ), name ); //$NON-NLS-1$
      }
    }
  }
}