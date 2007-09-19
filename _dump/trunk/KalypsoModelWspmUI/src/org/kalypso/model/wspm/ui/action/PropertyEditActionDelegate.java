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
package org.kalypso.model.wspm.ui.action;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.actions.ActionDelegate;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.ui.profil.wizard.propertyEdit.PropertyEditWizard;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ui.editor.gmleditor.ui.FeatureAssociationTypeElement;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;

/**
 * @author kimwerner
 */
public class PropertyEditActionDelegate extends ActionDelegate
{
  private IFeatureSelection m_selection;

  /**
   * @see org.eclipse.ui.actions.ActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  @Override
  public void selectionChanged( IAction action, ISelection selection )
  {
    m_selection = selection instanceof IFeatureSelection ? (IFeatureSelection) selection : null;
  }

  /**
   * @see org.eclipse.ui.actions.ActionDelegate#runWithEvent(org.eclipse.jface.action.IAction,
   *      org.eclipse.swt.widgets.Event)
   */
  @SuppressWarnings("unchecked")
  @Override
  public void runWithEvent( IAction action, Event event )
  {
    /* retrieve selected profiles, abort if none */
    final List<Feature> foundProfiles = new ArrayList<Feature>();
    final List<Feature> selectedProfiles = new ArrayList<Feature>();

    final Shell shell = event.display.getActiveShell();
    final Object fe = m_selection.getFirstElement();
    CommandableWorkspace ws=null;

    if( fe instanceof FeatureAssociationTypeElement )
    {
      final  FeatureAssociationTypeElement fate = (FeatureAssociationTypeElement) m_selection.getFirstElement();
      final IRelationType rt = fate.getAssociationTypeProperty();
      final Feature parentFeature = fate.getParentFeature();
      if( rt.isList() )
        foundProfiles.addAll( (FeatureList) parentFeature.getProperty( rt ) );
      else
      {
        foundProfiles.add( (Feature) parentFeature.getProperty( rt ) );
      }
      ws=m_selection.getWorkspace( parentFeature );
    }
    else if( fe instanceof Feature )
    {
      final IRelationType rt = ((Feature) fe).getParentRelation();
      final Feature parentFeature = ((Feature) fe).getParent();
      selectedProfiles.addAll( m_selection.toList() );
      ws=m_selection.getWorkspace( parentFeature );
      if( rt.isList() )
        foundProfiles.addAll( (FeatureList) parentFeature.getProperty( rt ) );
      else
      {
        foundProfiles.add( (Feature) parentFeature.getProperty( rt ) );
      }
    }

    if( foundProfiles.size() == 0 )
    {
      MessageDialog.openWarning( shell, "Feature Werte überschreiben", "Es wurden keine Profile in der Selektion gefunden." );
      return;
    }
    final IWizard propertyEditWizard = new PropertyEditWizard(ws, foundProfiles, selectedProfiles );

    /* show wizard */
    final WizardDialog2 dialog = new WizardDialog2( shell, propertyEditWizard );
    dialog.setRememberSize( true );
    dialog.open();
  }

}
