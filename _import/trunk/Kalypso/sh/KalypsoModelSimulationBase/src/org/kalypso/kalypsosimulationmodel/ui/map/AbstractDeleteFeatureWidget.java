/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.kalypsosimulationmodel.ui.map;

import javax.xml.namespace.QName;

import org.kalypso.contribs.eclipse.swt.awt.SWT_AWT_Utilities;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.command.CompositeCommand;
import org.kalypso.ogc.gml.command.DeleteFeatureCommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.feature.Feature;

/**
 * This widget lets the user grab features and then deletes them.
 * 
 * @author Gernot Belger
 */
public abstract class AbstractDeleteFeatureWidget extends AbstractSelectFeatureWidget
{
  public AbstractDeleteFeatureWidget( final String name, final String toolTip, final boolean allowMultipleSelection, final QName qnameToDelete, final QName geomQName )
  {
    super( name, toolTip, allowMultipleSelection, qnameToDelete, geomQName );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.ui.map.AbstractSelectFeatureWidget#flowRelationGrabbed(org.kalypso.ogc.gml.mapmodel.CommandableWorkspace,
   *      org.kalypsodeegree.model.feature.Feature[])
   */
  @Override
  protected void flowRelationGrabbed( final CommandableWorkspace workspace, final Feature[] selectedFeatures ) throws Exception
  {
    if( !SWT_AWT_Utilities.showSwtMessageBoxConfirm( "Objekte l�schen", "Selektierte Objekte werden gel�scht. Sind Sie sicher?" ) )
      return;

    /* Select the feature */
    final IFeatureSelectionManager selectionManager = getMapPanel().getSelectionManager();

    final CompositeCommand compositeCommand = new CompositeCommand( "Objekte l�schen" );
    for( final Feature featureToRemove : selectedFeatures )
    {
      selectionManager.changeSelection( new Feature[] { featureToRemove }, new EasyFeatureWrapper[] {} );

      final Feature parent = featureToRemove.getParent();
      final IRelationType parentRelation = featureToRemove.getParentRelation();
      final DeleteFeatureCommand command = new DeleteFeatureCommand( workspace, parent, parentRelation, featureToRemove );
      compositeCommand.addCommand( command );
    }

    workspace.postCommand( compositeCommand );
  }
}
