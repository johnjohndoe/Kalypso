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
package org.kalypso.kalypsomodel1d2d.schema.binding.model;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsosimulationmodel.core.VersionedModel;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * {@link Feature_Impl} based default implementation of {@link IControlModelGroup}.
 * 
 * @author Patrice Congo
 * @author Dejan Antanaskovic
 * 
 */
public class ControlModelGroup extends VersionedModel implements IControlModelGroup
{
  public ControlModelGroup( Object parent, IRelationType parentRelation, IFeatureType ft, String id, Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  public final static QName WB1D2DCONTROL_FP_MODEL_COLLECTION = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "controlModelCollection" ); //$NON-NLS-1$

  // Control model
  public final static QName WB1D2DCONTROL_F_MODEL_GROUP = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "ControlModelGroup" ); //$NON-NLS-1$

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModelGroup#getModel1D2DCollection()
   */
  @Override
  public IControlModel1D2DCollection getModel1D2DCollection( )
  {
    Object controlModelCollection = getProperty( WB1D2DCONTROL_FP_MODEL_COLLECTION );
    if( controlModelCollection == null )
    {
      final GMLWorkspace workspace = getWorkspace();
      final IRelationType parentRelation = (IRelationType) getFeatureType().getProperty( ControlModelGroup.WB1D2DCONTROL_FP_MODEL_COLLECTION );
      controlModelCollection = workspace.createFeature( this, parentRelation, parentRelation.getTargetFeatureType(), -1 );
      setProperty( ControlModelGroup.WB1D2DCONTROL_FP_MODEL_COLLECTION, controlModelCollection );
    }
    return (IControlModel1D2DCollection) ((Feature) controlModelCollection).getAdapter( IControlModel1D2DCollection.class );
  }

}
