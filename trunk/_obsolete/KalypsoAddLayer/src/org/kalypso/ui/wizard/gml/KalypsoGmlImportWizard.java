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
package org.kalypso.ui.wizard.gml;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoAddLayerPlugin;
import org.kalypso.ui.action.AddThemeCommand;
import org.kalypso.ui.editor.gmleditor.ui.FeatureAssociationTypeElement;
import org.kalypso.ui.wizard.IKalypsoDataImportWizard;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.binding.commons.NamedFeatureHelper;
import org.kalypsodeegree_impl.model.feature.FeaturePath;

/**
 * @author Kuepferle
 */
public class KalypsoGmlImportWizard extends Wizard implements IKalypsoDataImportWizard
{
  private ICommandTarget m_outlineviewer;

  private IKalypsoLayerModell m_mapModel;

  private GmlFileImportPage m_page;

  @Override
  public void addPages( )
  {
    m_page = new GmlFileImportPage( "GML:importPage", "Hinzufügen einer GML-Datei (im Workspace) zu einer Karte", ImageProvider.IMAGE_UTIL_UPLOAD_WIZ );
    m_page.setProjectSelection( m_mapModel.getProject() );

    addPage( m_page );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    try
    {
      final ICommand[] commands = getCommands( m_mapModel );

      for( final ICommand command : commands )
      {
        m_outlineviewer.postCommand( command, null );
      }
    }
    catch( final Throwable e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoAddLayerPlugin.getDefault().getLog().log( status );
      ErrorDialog.openError( getShell(), getWindowTitle(), "Thema konnte nicht hinzugefügt werden: ", status );
      return false;
    }

    return true;
  }

  private ICommand[] getCommands( final IKalypsoLayerModell model )
  {
    final String source = m_page.getSource();
    final IStructuredSelection selection = m_page.getSelection();
    final GMLWorkspace workspace = m_page.getWorkspace();

    final Object firstElement = selection.getFirstElement();
    final List<String> pathList = new ArrayList<String>();
    final List<String> titleList = new ArrayList<String>();
    if( firstElement instanceof Feature )
    {
      // create featurepath for element
      final Feature feature = (Feature) firstElement;
      final FeaturePath featurepath = workspace.getFeaturepathForFeature( feature );
      final IFeatureType ft = feature.getFeatureType();
      // find title
      String title = NamedFeatureHelper.getName( feature );
      if( title == null || title.length() < 1 )
        title = ft.getAnnotation().getLabel();
      pathList.add( featurepath.toString() );
      titleList.add( title );
    }
    else if( firstElement instanceof FeatureAssociationTypeElement )
    {
      // create featurepath for association
      final FeatureAssociationTypeElement link = (FeatureAssociationTypeElement) firstElement;
      final Feature parent = link.getParentFeature();
      final FeaturePath parentFeaturePath = workspace.getFeaturepathForFeature( parent );
      final IRelationType ftp = link.getAssociationTypeProperty();

      final IFeatureType associationFeatureType = ftp.getTargetFeatureType();
      final IFeatureType[] associationFeatureTypes = GMLSchemaUtilities.getSubstituts( associationFeatureType, null, false, true );

      for( final IFeatureType ft : associationFeatureTypes )
      {
        final String title = ft.getAnnotation().getLabel();
        final String ftpName = ftp.getQName().getLocalPart();
        final String ftName = ft.getQName().getLocalPart();
        final FeaturePath path = new FeaturePath( parentFeaturePath, ftpName + "[" + ftName + "]" );
        pathList.add( path.toString() );
        titleList.add( title );
      }
    }

    final ICommand[] result = new ICommand[pathList.size()];
    final Iterator titleIterator = titleList.iterator();
    int pos = 0;
    for( final Iterator pathIterator = pathList.iterator(); pathIterator.hasNext(); pos++ )
    {
      final String title = (String) titleIterator.next();
      final String featurePath = (String) pathIterator.next();
      result[pos] = new AddThemeCommand( model, title, "gml", featurePath, source );
    }
    return result;
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
    setWindowTitle( "GML Datei" );
  }

  /**
   * @see org.kalypso.ui.wizard.IKalypsoDataImportWizard#setMapModel(org.kalypso.ogc.gml.IKalypsoLayerModell)
   */
  public void setMapModel( final IKalypsoLayerModell modell )
  {
    m_mapModel = modell;
  }

}