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
package org.kalypso.wizards.export2d;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.kalypsomodel1d2d.conv.Gml2RMA10SConv;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;
import org.kalypso.ui.KalypsoGisPlugin;

public class ExportWizard extends Wizard implements INewWizard
{
  private FileSelectWizardPage m_page1;

  protected IProject m_project = null;

  public ExportWizard( )
  {
    final IDialogSettings settings = KalypsoGisPlugin.getDefault().getDialogSettings();

    IDialogSettings section = settings.getSection( "ExportAsFileWizard" ); //$NON-NLS-1$
    if( section == null )
    {
      section = settings.addNewSection( "ExportAsFileWizard" ); //$NON-NLS-1$
    }
    setDialogSettings( section );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    super.addPages();
    m_page1 = new FileSelectWizardPage( "fileselect", new String[] { "*.2d" } );
    addPage( m_page1 );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#createPageControls(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPageControls( final Composite pageContainer )
  {
    setWindowTitle( "Als Datei exportieren" );
    setNeedsProgressMonitor( true );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    final File exportFile = new File( m_page1.getFilePath() );
    final boolean exportMiddleNodes = m_page1.isSelectedExportMiddleNodes();
    final boolean exportRoughness = m_page1.isSelectedExportRoughessData();

    final ICoreRunnableWithProgress operation = new ICoreRunnableWithProgress()
    {
      public IStatus execute( IProgressMonitor monitor ) throws CoreException, InvocationTargetException
      {
        monitor.beginTask( "FE Netz exportieren", IProgressMonitor.UNKNOWN );

        final SzenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDefault().getDataProvider();
        final IFEDiscretisationModel1d2d discretisationModel = dataProvider.getModel( IFEDiscretisationModel1d2d.class );
        final IFlowRelationshipModel flowRelationshipModel = dataProvider.getModel( IFlowRelationshipModel.class );
        final IRoughnessClsCollection roughnessModel = exportRoughness ? dataProvider.getModel( IRoughnessClsCollection.class ) : null;

        final Gml2RMA10SConv converter = new Gml2RMA10SConv( discretisationModel, flowRelationshipModel, null, roughnessModel, null, true, exportMiddleNodes );

        try
        {
          converter.writeRMA10sModel( exportFile );
        }
        catch( IOException e )
        {
          e.printStackTrace();
          throw new InvocationTargetException( e );
        }
        return Status.OK_STATUS;
      }
    };

    final IStatus result = RunnableContextHelper.execute( getContainer(), true, true, operation );
    ErrorDialog.openError( getShell(), "FE-Netz exportieren", "Datei kann nicht erzeugt werden", result );

    return result.isOK();
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    // TODO Auto-generated method stub

  }
}
