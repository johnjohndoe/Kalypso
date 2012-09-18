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
package org.kalypso.model.wspm.tuhh.ui.actions;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.HandlerUtil;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureRelation;

/**
 * @author Gernot Belger
 */
public class CreateProfileMapHandler extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final ISelection selection = HandlerUtil.getCurrentSelectionChecked( event );
    if( !(selection instanceof IFeatureSelection) )
      throw new ExecutionException( "selection must be feature selection" ); //$NON-NLS-1$

    final Shell shell = HandlerUtil.getActiveShellChecked( event );

    final IFeatureSelection featureSelection = (IFeatureSelection) selection;

    /* retrieve selected profile-collections, abort if none */
    final Map<Feature, IRelationType> selectedProfiles = new HashMap<>();

    for( final Object selectedObject : featureSelection.toList() )
    {
      if( selectedObject instanceof Feature )
      {
        final Feature feature = (Feature) selectedObject;
        final IRelationType rt = (IRelationType) feature.getFeatureType().getProperty( TuhhReach.QNAME_MEMBER_REACHSEGMENT );
        selectedProfiles.put( feature, rt );
      }
      if( selectedObject instanceof IFeatureRelation )
      {
        final Feature feature = ((IFeatureRelation) selectedObject).getOwner();
        final IRelationType rt = ((IFeatureRelation) selectedObject).getPropertyType();
        selectedProfiles.put( feature, rt );
      }
    }

    if( selectedProfiles.size() == 0 )
    {
      MessageDialog.openWarning( shell, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.actions.CreateProfileMapHandler.0" ), Messages.getString( "org.kalypso.model.wspm.tuhh.ui.actions.CreateProfileMapHandler.1" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      return null;
    }

    final IWorkbenchPart activePart = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActivePart();
    org.kalypso.model.wspm.ui.action.CreateProfileMapAction.createAndOpenMap( activePart, selectedProfiles );

    return null;
  }
}