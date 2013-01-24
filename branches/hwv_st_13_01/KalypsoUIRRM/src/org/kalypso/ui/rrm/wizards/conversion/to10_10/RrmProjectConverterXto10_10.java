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
package org.kalypso.ui.rrm.wizards.conversion.to10_10;

import java.io.File;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.kalypsosimulationmodel.ui.calccore.CalcCoreUtils;
import org.kalypso.kalypsosimulationmodel.ui.calccore.ChooseExeDialog;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.module.conversion.AbstractProjectConverter;
import org.kalypso.ui.rrm.i18n.Messages;
import org.osgi.framework.Version;


/**
 * @author Gernot Belger
 */
public class RrmProjectConverterXto10_10 extends AbstractProjectConverter
{
  private static File NEUESTE = new File( Messages.getString("RrmProjectConverterXto10.10.0") ); //$NON-NLS-1$

  private static File EXE_DO_NOT_CHANGE = new File( Messages.getString("RrmProjectConverterXto10.10.1") ); //$NON-NLS-1$

  private final File m_sourceDir;

  private final File m_targetDir;

  private String m_chosenExe;

  public RrmProjectConverterXto10_10( final File sourceDir, final File targetDir, final Version targetVersion )
  {
    super( String.format( Messages.getString( "RrmProjectConverter103to230_0" ), sourceDir.getName(), targetVersion ) ); //$NON-NLS-1$

    m_sourceDir = sourceDir;
    m_targetDir = targetDir;
  }

  /**
   * @see org.kalypso.module.conversion.IProjectConverter#preConversion(org.eclipse.swt.widgets.Shell)
   */
  @Override
  public IStatus preConversion( final Shell shell )
  {
    final String title = Messages.getString("RrmProjectConverterXto10.10.2"); //$NON-NLS-1$

    /* Always call this in order to provoke the download error message */
    File[] availableExeFiles = CalcCoreUtils.checkExecutablesAvailable( shell, NaModelConstants.EXE_PATTERN, title );
    availableExeFiles = (File[]) ArrayUtils.add( availableExeFiles, NEUESTE );
    availableExeFiles = (File[]) ArrayUtils.add( availableExeFiles, EXE_DO_NOT_CHANGE );

    final ChooseExeDialog dialog = new ChooseExeDialog( shell, availableExeFiles );
    dialog.setTitle( title );
    if( dialog.open() != Window.OK )
      return Status.CANCEL_STATUS;

    final Object[] result = dialog.getResult();
    if( result.length == 0 )
      return Status.CANCEL_STATUS;

    final File chosenExe = (File) result[0];
    m_chosenExe = findChosenExe( chosenExe );

    return Status.OK_STATUS;
  }

  private String findChosenExe( final File chosenExe )
  {
    if( chosenExe == NEUESTE )
      return CalcCoreUtils.VERSION_NEUESTE;

    if( chosenExe == EXE_DO_NOT_CHANGE )
      return null;

    final Matcher exeMatcher = Pattern.compile( NaModelConstants.EXE_PATTERN ).matcher( chosenExe.getName() );
    exeMatcher.matches();

    return exeMatcher.group( 1 );
  }

  /**
   * @see org.kalypso.ui.rrm.wizards.conversion.AbstractLoggingOperation#doExecute(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  protected void doExecute( final IProgressMonitor monitor ) throws Exception
  {
    final String projectName = m_sourceDir.getName();
    monitor.beginTask( String.format( Messages.getString("RrmProjectConverter103to230_1"), projectName ), 100 ); //$NON-NLS-1$

    try
    {
      final BasicModelConverter basicModelConverter = new BasicModelConverter( m_sourceDir, m_targetDir );
      monitor.subTask( Messages.getString("RrmProjectConverter103to230_2") ); //$NON-NLS-1$
      final IStatus basicStatus = basicModelConverter.execute( new SubProgressMonitor( monitor, 33 ) );
      getLog().add( basicStatus );

      monitor.subTask( Messages.getString("RrmProjectConverter103to230_3") ); //$NON-NLS-1$
      final CalcCasesConverter casesConverter = new CalcCasesConverter( m_sourceDir, m_targetDir, m_chosenExe );
      final IStatus calcCaseStatus = casesConverter.execute( new SubProgressMonitor( monitor, 67 ) );
      getLog().add( calcCaseStatus );
    }
    finally
    {
      monitor.done();
    }
  }
}
