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
package org.kalypso.ui.rrm.internal.conversion.to12_02;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.module.conversion.AbstractLoggingOperation;
import org.kalypso.ui.rrm.internal.i18n.Messages;

/**
 * Converts calc cases to scenarios.
 *
 * @author Gernot Belger
 * @author Holger Albert
 */
public class CalcCasesConverter extends AbstractLoggingOperation
{
  /**
   * The global conversion data.
   */
  private final GlobalConversionData m_globalData;

  /**
   * @param globalData
   *          The global conversion data.
   */
  public CalcCasesConverter( final GlobalConversionData globalData )
  {
    super( Messages.getString( "CalcCasesConverter_0" ) ); //$NON-NLS-1$

    m_globalData = globalData;
  }

  @Override
  protected void doExecute( final IProgressMonitor monitor ) throws InterruptedException
  {
    try
    {
      /* Monitor. */
      final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString( "CalcCasesConverter_1" ), 101 ); //$NON-NLS-1$

      /* Find all calc cases. */
      final CalcCaseConvertWalker walker = new CalcCaseConvertWalker( m_globalData.getSourceDir() );
      final File[] calcCases = walker.execute();

      /* Monitor. */
      ProgressUtilities.worked( progress, 1 );
      progress.setWorkRemaining( calcCases.length );

      /* Convert all calc cases into scenarios. */
      for( int i = 0; i < calcCases.length; i++ )
      {
        /* Monitor. */
        progress.subTask( String.format( Messages.getString( "CalcCasesConverter_2" ), calcCases[i].getName(), i + 1, calcCases.length ) ); //$NON-NLS-1$

        /* Determine the directories. */
        final File sourceDir = calcCases[i];
        final File targetDir = determineTargetDir( sourceDir );

        try
        {
          /* Convert the calc case to a scenario. */
          final CalcCaseConverter calcCaseConverter = new CalcCaseConverter( sourceDir, targetDir, m_globalData );
          final IStatus status = calcCaseConverter.execute( progress.newChild( 1 ) );
          getLog().add( status );
        }
        catch( final CoreException ce )
        {
          final IStatus status = ce.getStatus();
          if( status.matches( IStatus.CANCEL ) )
            throw new InterruptedException( Messages.getString( "CalcCasesConverter_3" ) ); //$NON-NLS-1$

          getLog().add( status );
        }
        catch( final InvocationTargetException e )
        {
          getLog().add( IStatus.ERROR, Messages.getString( "CalcCasesConverter_4" ), e.getTargetException(), calcCases[i].getName() ); //$NON-NLS-1$
        }
        catch( final Throwable e )
        {
          getLog().add( IStatus.ERROR, Messages.getString( "CalcCasesConverter_4" ), e, calcCases[i].getName() ); //$NON-NLS-1$
        }
      }
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      getLog().add( IStatus.ERROR, Messages.getString( "CalcCasesConverter_5" ), e ); //$NON-NLS-1$
    }
  }

  /**
   * This function determines the directory of the scenario.
   *
   * @param sourceDir
   *          The directory of the source calc case.
   * @return The directory of the scenario.
   */
  private File determineTargetDir( final File sourceDir )
  {
    final File baseScenarioDir = m_globalData.getBaseScenarioDir();
    final IPath baseScenarioPath = Path.fromOSString( baseScenarioDir.getAbsolutePath() );
    // TODO Better take this segment from the (workflow) extension point?
    final IPath scenariosPath = baseScenarioPath.append( ScenariosExclusionFileFilter.SCENARIOS_FOLDER );
    final String scenarioName = findScenarioName( sourceDir );
    final IPath scenarioPath = scenariosPath.append( scenarioName );
    return scenarioPath.toFile();
  }

  /**
   * This function adds all segments after the segment 'Rechenvarianten' to a string. They will be separated by '_'.
   *
   * @param sourceDir
   *          The directory of the source calc case.
   * @return The name of the scenario.
   */
  private String findScenarioName( final File sourceDir )
  {
    /* All segments after 'Rechenvarianten'. */
    final List<String> segmentsAfter = new ArrayList<>();

    /* Get the source path. */
    final IPath sourcePath = Path.fromOSString( sourceDir.getAbsolutePath() );

    /* Found the segment 'Rechenvarianten'? */
    boolean foundCalcCasesSegment = false;

    /* Get all segments. */
    final String[] segments = sourcePath.segments();
    for( final String segment : segments )
    {
      if( foundCalcCasesSegment )
      {
        segmentsAfter.add( segment );
        continue;
      }

      if( segment.equals( INaProjectConstants.FOLDER_RECHENVARIANTEN ) )
        foundCalcCasesSegment = true;
    }

    return StringUtils.join( segmentsAfter, "_" ); //$NON-NLS-1$
  }
}