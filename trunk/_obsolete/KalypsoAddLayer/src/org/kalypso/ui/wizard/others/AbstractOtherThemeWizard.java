/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.ui.wizard.others;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ui.KalypsoAddLayerPlugin;
import org.kalypso.ui.wizard.IKalypsoDataImportWizard;

/**
 * @author Gernot Belger
 */
public abstract class AbstractOtherThemeWizard extends Wizard implements IKalypsoDataImportWizard
{
  private final ThemeNameWizardPage m_themeNameWizardPage;

  private ICommandTarget m_outlineviewer;

  private IKalypsoLayerModell m_mapModel;

  public AbstractOtherThemeWizard( final ThemeNameWizardPage themeNameWizardPage )
  {
    m_themeNameWizardPage = themeNameWizardPage;

    m_themeNameWizardPage.setDescription( "Geben Sie den Namen für das neue Thema ein" );
  }

  @Override
  public void addPages( )
  {
    addPage( m_themeNameWizardPage );
  }

  public void setMapModel( final IKalypsoLayerModell modell )
  {
    m_mapModel = modell;
  }

  public void setCommandTarget( final ICommandTarget commandTarget )
  {
    m_outlineviewer = commandTarget;
  }

  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    // nothing
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    final IKalypsoLayerModell mapModell = m_mapModel;

    final String themeName = m_themeNameWizardPage.getThemeName();

    final ICommandTarget outlineviewer = m_outlineviewer;
    final ICoreRunnableWithProgress operation = new ICoreRunnableWithProgress()
    {
      public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException
      {
        try
        {
          if( mapModell == null )
            return StatusUtilities.createErrorStatus( "Keine Karte vorhanden" );

          final ICommand command = createCommand( mapModell, themeName );
          outlineviewer.postCommand( command, null );
        }
        catch( final Throwable t )
        {
          throw new InvocationTargetException( t );
        }

        return Status.OK_STATUS;
      }

    };

    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, operation );
    KalypsoAddLayerPlugin.getDefault().getLog().log( status );
    ErrorDialog.openError( getShell(), getWindowTitle(), "Fehler beim Hinzufügen des Themas", status );

    return status.isOK();
  }

  protected abstract ICommand createCommand( IKalypsoLayerModell mapModell, String themeName );

}