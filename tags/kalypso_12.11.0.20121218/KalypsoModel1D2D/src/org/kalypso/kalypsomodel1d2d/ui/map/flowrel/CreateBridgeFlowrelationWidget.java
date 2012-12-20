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
package org.kalypso.kalypsomodel1d2d.ui.map.flowrel;

import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBridgeFlowRelation;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public class CreateBridgeFlowrelationWidget extends AbstractCreateFlowrelationWidget
{
  public CreateBridgeFlowrelationWidget( )
  {
    super( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.CreateBridgeFlowrelationWidget.0"), Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.CreateBridgeFlowrelationWidget.1"), IBridgeFlowRelation.QNAME ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.AbstractCreateFlowrelationWidget#createNewFeature(org.kalypso.ogc.gml.mapmodel.CommandableWorkspace,
   *      org.kalypsodeegree.model.feature.Feature, org.kalypso.gmlschema.property.relation.IRelationType,
   *      org.kalypso.gmlschema.feature.IFeatureType)
   */
  @Override
  protected IBridgeFlowRelation createNewFeature( final CommandableWorkspace workspace, final Feature parentFeature, final IRelationType parentRelation, final Feature modelElement )
  {
    // TODO: use newAdd method in FeatureWrapperCollection instead?
    final IFeatureType newFT = GMLSchemaUtilities.getFeatureTypeQuiet( IBridgeFlowRelation.QNAME );
    final Feature newFeature = workspace.createFeature( parentFeature, parentRelation, newFT, -1 );
    final IBridgeFlowRelation buildingRelation = (IBridgeFlowRelation) newFeature.getAdapter( IBridgeFlowRelation.class );
    /* Call getObservation once to initialize it */
    buildingRelation.getBuildingObservation();
    return buildingRelation;
  }

//  /**
//   * Return one 1D-Element.
//   *
//   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.AbstractCreateFlowrelationWidget#findModelElementFromCurrentPosition(org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d,
//   *      org.kalypsodeegree.model.geometry.GM_Point, double)
//   */
//  @Override
//  protected Feature findModelElementFromCurrentPosition( final IFEDiscretisationModel1d2d discModel, final GM_Point currentPos, final double grabDistance )
//  {
//    return discModel.find1DElement( currentPos, grabDistance );
//  }
}
