package org.kalypso.ui.wizard.raster;

import java.lang.reflect.InvocationTargetException;
import java.net.URL;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoAddLayerPlugin;
import org.kalypso.ui.action.AddThemeCommand;
import org.kalypso.ui.wizard.IKalypsoDataImportWizard;

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

public class ImportRasterSourceWizard extends Wizard implements IKalypsoDataImportWizard
{

  private ICommandTarget m_outlineviewer;

  private ImportRasterSourceWizardPage m_page;

  private IProject m_project;

  private URL m_mapContextURL;

  private IMapModell m_mapModel;

  public ImportRasterSourceWizard( )
  {
    super();
  }

  /**
   * @see org.kalypso.ui.wizard.IKalypsoDataImportWizard#setMapModel(org.kalypso.ogc.gml.mapmodel.IMapModell)
   */
  public void setMapModel( final IMapModell modell )
  {
    m_mapModel = modell;
    m_project = m_mapModel.getProject();
    m_mapContextURL = ((GisTemplateMapModell) modell).getContext();
  }

  @Override
  public void addPages( )
  {
    m_page = new ImportRasterSourceWizardPage( "Add RasterDataModel", "Add raster theme", ImageProvider.IMAGE_KALYPSO_ICON_BIG );
    if( m_project != null )
      m_page.setProject( m_project );
    addPage( m_page );

  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    final IPath filePath = m_page.getFilePath();
    final boolean useDefaultStyle = m_page.checkDefaultStyle();
    final GisTemplateMapModell mapModell = (GisTemplateMapModell) m_mapModel;
    final String source = getRelativeProjectPath( filePath );
    final String stylePath = useDefaultStyle ? null : getRelativeProjectPath( m_page.getStylePath() );
    final String styleName = useDefaultStyle ? null : m_page.getStyleName();

    final ICoreRunnableWithProgress operation = new ICoreRunnableWithProgress()
    {
      public IStatus execute( IProgressMonitor monitor ) throws InvocationTargetException
      {
        try
        {
          // TODO: analyse gml in order to set featurePath
          // - is root feature a grid
          // - find all properties pointing to a grid

          if( mapModell == null )
            return StatusUtilities.createErrorStatus( "Keine Karte vorhanden" );

          final String themeName = filePath.lastSegment();
          final String type = "gml";
          final String featurePath = "";
          final AddThemeCommand command = new AddThemeCommand( mapModell, themeName, type, featurePath, source, "sld", styleName, stylePath, "simple" );
          m_outlineviewer.postCommand( command, null );
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
    ErrorDialog.openError( getShell(), getWindowTitle(), "Fehler beim Hinzufügen des Rasterthemas", status );

    return status.isOK();
  }

  private String getRelativeProjectPath( final IPath path )
  {
    return "project:/" + path.removeFirstSegments( 1 ).toString();
  }

  /**
   * @see org.kalypso.ui.wizard.data.IKalypsoDataImportWizard#setOutlineViewer(org.kalypso.ogc.gml.outline.GisMapOutlineViewer)
   */
  public void setCommandTarget( final ICommandTarget commandTarget )
  {
    m_outlineviewer = commandTarget;
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    // nothing
  }
}