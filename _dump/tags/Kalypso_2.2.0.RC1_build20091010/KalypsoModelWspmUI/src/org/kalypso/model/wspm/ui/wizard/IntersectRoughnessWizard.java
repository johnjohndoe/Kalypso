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
package org.kalypso.model.wspm.ui.wizard;

import java.lang.reflect.InvocationTargetException;
import java.net.URL;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.eclipse.jface.wizard.ArrayChooserPage;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.model.wspm.core.gml.assignment.AssignmentBinder;
import org.kalypso.model.wspm.core.profil.filter.IProfilePointFilter;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.wizard.FeatureThemeWizardUtilitites.FOUND_PROFILES;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.editor.gmleditor.ui.GMLLabelProvider;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Gernot Belger
 */
public class IntersectRoughnessWizard extends Wizard
{
  private final GMLLabelProvider m_chooserPageLabelProvider = new GMLLabelProvider();

  private ArrayChooserPage m_profileChooserPage;

  private IntersectRoughnessPage m_roughnessIntersectPage;

  private final FOUND_PROFILES m_foundProfiles;

  public IntersectRoughnessWizard( final FOUND_PROFILES foundProfiles )
  {
    m_foundProfiles = foundProfiles;

    setWindowTitle( org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.wizard.IntersectRoughnessWizard.0") ); //$NON-NLS-1$
    setNeedsProgressMonitor( true );
    setDialogSettings( PluginUtilities.getDialogSettings( KalypsoModelWspmUIPlugin.getDefault(), getClass().getName() ) );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    m_profileChooserPage = new ArrayChooserPage( m_foundProfiles.foundProfiles, new Object[] {}, m_foundProfiles.selectedProfiles, 0, "profileFeaturesChooserPage", org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.wizard.IntersectRoughnessWizard.1"), null ); //$NON-NLS-1$ //$NON-NLS-2$
    m_profileChooserPage.setLabelProvider( m_chooserPageLabelProvider );
    m_profileChooserPage.setMessage( org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.wizard.IntersectRoughnessWizard.2") ); //$NON-NLS-1$

    m_roughnessIntersectPage = new IntersectRoughnessPage( m_foundProfiles.theme.getMapModell() );

    addPage( m_profileChooserPage );
    addPage( m_roughnessIntersectPage );

    super.addPages();
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#dispose()
   */
  @Override
  public void dispose( )
  {
    m_chooserPageLabelProvider.dispose();

    super.dispose();
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    final Object[] choosen = m_profileChooserPage.getChoosen();
    if( choosen.length == 0 )
      return true;

    final FeatureList polygoneFeatures = m_roughnessIntersectPage.getPolygoneFeatures();
    final IPropertyType polygoneGeomType = m_roughnessIntersectPage.getPolygoneGeomProperty();
    final IPropertyType polygoneValueType = m_roughnessIntersectPage.getPolygoneValueProperty();
    final IPath assignmentPath = m_roughnessIntersectPage.getAssignmentPath();
    final IProfilePointFilter[] pointFilters = m_roughnessIntersectPage.getSelectedPointFilter();

    final ICommandTarget target = m_foundProfiles.theme;

    final ICoreRunnableWithProgress runnable = new ICoreRunnableWithProgress()
    {
      public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException
      {
        monitor.beginTask( org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.wizard.IntersectRoughnessWizard.3"), 1 + choosen.length ); //$NON-NLS-1$

        try
        {
          /* Load assignment */
          monitor.subTask( org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.wizard.IntersectRoughnessWizard.4") ); //$NON-NLS-1$
          final IWorkspace workspace = ResourcesPlugin.getWorkspace();
          final IFile assignmentFile = workspace.getRoot().getFile( assignmentPath );
          final URL assignmentUrl = ResourceUtilities.createURL( assignmentFile );

          final GMLWorkspace assignmentWorkspace = GmlSerializer.createGMLWorkspace( assignmentUrl, null );
          final AssignmentBinder assignment = new AssignmentBinder( assignmentWorkspace );
          monitor.worked( 1 );

          final RoughnessIntersector intersector = new RoughnessIntersector( choosen, polygoneFeatures, polygoneGeomType, polygoneValueType, assignment, pointFilters );
          final FeatureChange[] changes = intersector.intersect( new SubProgressMonitor( monitor, choosen.length ) );
          if( changes.length > 0 )
          {
            final GMLWorkspace gmlworkspace = changes[0].getFeature().getWorkspace();
            final ICommand command = new ChangeFeaturesCommand( gmlworkspace, changes );
            target.postCommand( command, null );
          }
        }
        catch( final Exception e )
        {
          throw new InvocationTargetException( e );
        }
        finally
        {
          monitor.done();
        }

        return Status.OK_STATUS;
      }
    };

    final IStatus status = RunnableContextHelper.execute( getContainer(), true, true, runnable );
    ErrorDialog.openError( getShell(), getWindowTitle(), org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.wizard.IntersectRoughnessWizard.5"), status ); //$NON-NLS-1$

    return status.isOK();
  }

}
