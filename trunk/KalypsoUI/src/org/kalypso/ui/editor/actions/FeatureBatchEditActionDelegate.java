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
package org.kalypso.ui.editor.actions;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IActionDelegate;
import org.kalypso.commons.command.ICommand;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.featureview.FeatureChange;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Action delegate class for feature batch editing
 * 
 * @author Stefan Kurzbach
 */
public class FeatureBatchEditActionDelegate implements IActionDelegate
{

  private IFeatureSelection m_selection;

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    if( m_selection == null )
      return;
    final Feature[] fes = FeatureSelectionHelper.getFeatures( m_selection );
    if( fes.length == 0 )
      return;

    final Feature focusedFeature = m_selection.getFocusedFeature();
    final IPropertyType focusedProperty = m_selection.getFocusedProperty();
    final CommandableWorkspace workspace = m_selection.getWorkspace( focusedFeature );

//    final ICommand editCommand = new EditFeatureValueCommand( workspace, fes[0], focusedProperty );
//    final CompositeCommand compositeCommand = new CompositeCommand( editCommand.getDescription(), editCommand );

    FeatureChange[] changeArray = new FeatureChange[fes.length];
    for( int i = 0; i < fes.length; i++ )
    {
      changeArray[i] = new RelativeFeatureChange(fes[i],focusedProperty);
//      compositeCommand.addCommand( new EditFeatureValueCommand( workspace, fes[i], focusedProperty ) );
    }

    final ChangeFeaturesCommand changeFeaturesCommand = new ChangeFeaturesCommand(workspace, changeArray);

    try
    {
      workspace.postCommand( changeFeaturesCommand );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, "", e );

      // we are in the ui-thread so we get a shell here
      final Shell shell = Display.getCurrent().getActiveShell();
      if( shell != null )
        ErrorDialog.openError( shell, action.getText(), changeFeaturesCommand.getDescription(), status );
    }

  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    if( selection instanceof IFeatureSelection )
    {
      m_selection = (IFeatureSelection) selection;
    }
  }

}
