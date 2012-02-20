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
package org.kalypso.ui.rrm.internal.conversion;

import java.io.File;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.kalypsosimulationmodel.ui.calccore.CalcCoreUtils;
import org.kalypso.kalypsosimulationmodel.ui.calccore.ChooseExeDialog;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.ui.rrm.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class ChooseExeConverter
{
  private static File NEUESTE = new File( Messages.getString( "RrmProjectConverterXto10.10.0" ) ); //$NON-NLS-1$

  private static File EXE_DO_NOT_CHANGE = new File( Messages.getString( "RrmProjectConverterXto10.10.1" ) ); //$NON-NLS-1$

  private String m_chosenExe;

  public IStatus execute( final Shell shell )
  {
    final String title = Messages.getString( "RrmProjectConverterXto10.10.2" ); //$NON-NLS-1$

    /* Always call this in order to provoke the download error message */
    File[] availableExeFiles = CalcCoreUtils.checkExecutablesAvailable( shell, NaModelConstants.EXE_PATTERN, title );
    availableExeFiles = ArrayUtils.add( availableExeFiles, NEUESTE );
    availableExeFiles = ArrayUtils.add( availableExeFiles, EXE_DO_NOT_CHANGE );

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

  public String getChosenExe( )
  {
    return m_chosenExe;
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
}