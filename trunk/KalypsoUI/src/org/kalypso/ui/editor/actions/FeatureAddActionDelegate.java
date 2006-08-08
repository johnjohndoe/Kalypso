/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.ui.editor.actions;

import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * FeatureAddActionDelegate
 * 
 * @author doemming (24.05.2005)
 */
public class FeatureAddActionDelegate implements IEditorActionDelegate, IViewActionDelegate
{
  private IFeatureSelection m_selection = null;

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    if( m_selection != null )
    {
      final Feature firstFeature = FeatureSelectionHelper.getFirstFeature( m_selection );
      if( firstFeature == null )
        return;

      final CommandableWorkspace workspace = m_selection.getWorkspace( firstFeature );
      final Feature parentFeature = m_selection.getParentFeature( firstFeature );

      // TODO change featurelist and remove cast
      // TODO ask for IFeatureType (substitutiongroup)

      final IRelationType ftp = m_selection.getParentFeatureProperty( firstFeature );

      int pos = 0; // TODO get pos from somewhere
      final AddFeatureCommand command = new AddFeatureCommand( workspace, ftp.getTargetFeatureType(),parentFeature, ftp, pos, null, null );
      try
      {
        workspace.postCommand( command );
      }
      catch( final Exception e )
      {
        e.printStackTrace();

        final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, "", e );

        // we are in the ui-thread so we get a shell here
        final Shell shell = Display.getCurrent().getActiveShell();
        if( shell != null )
          ErrorDialog.openError( shell, action.getText(), "Fehler beim Hinzufügen des Features", status );
      }
    }
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( IAction action, ISelection selection )
  {
    action.setEnabled( false );
    m_selection = null;

    if( !selection.isEmpty() && selection instanceof IFeatureSelection )
    {
      m_selection = (IFeatureSelection) selection;

      final Feature selectedFeature = FeatureSelectionHelper.getFirstFeature( m_selection );
      // it is always a Feature (objectcontribution)
      final Feature parentFeature = m_selection.getParentFeature( selectedFeature );
      if( selectedFeature != null && parentFeature != null )
        action.setEnabled( checkMaxOccurs( parentFeature, selectedFeature ) );
    }
  }

  private boolean checkMaxOccurs( Feature feature, Feature featureToCheckOccurence )
  {
    int maxOccurs = -1;
    int size = -1;
    IFeatureType featureType = feature.getFeatureType();
    IPropertyType[] properties = featureType.getProperties();
    int[] pos = FeatureHelper.getPositionOfAllAssociations( feature );
    if( pos.length > 0 )
    {
      for( int i = 0; i < pos.length; i++ )
      {
        Object property = feature.getProperty( pos[i] );
        IRelationType ftp = (IRelationType) properties[pos[i]];
        maxOccurs = ftp.getMaxOccurs();
        if( property instanceof Feature )
        {
          Feature f = (Feature) property;
          if( f.equals( featureToCheckOccurence ) )
            return false;

        }
        if( property instanceof List )
        {
          size = ((List) property).size();
          if( maxOccurs == IPropertyType.UNBOUND_OCCURENCY )
            return true;
          else if( maxOccurs < size )
            return false;
        }
      }

    }

    return true;
  }

  /**
   * @see org.eclipse.ui.IEditorActionDelegate#setActiveEditor(org.eclipse.jface.action.IAction, org.eclipse.ui.IEditorPart)
   */
  public void setActiveEditor( IAction action, IEditorPart targetEditor )
  {
  }

  /**
   * @see org.eclipse.ui.IViewActionDelegate#init(org.eclipse.ui.IViewPart)
   */
  public void init( IViewPart view )
  {
  }
}
