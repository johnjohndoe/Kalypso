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
package org.kalypso.model.wspm.tuhh.ui.actions.dtm;

import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemePredicate;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureRelation;

/**
 * @author Gernot Belger
 */
public class ProfileThemePredicate implements IKalypsoThemePredicate
{
  @Override
  public boolean decide( final IKalypsoTheme theme )
  {
    if( !(theme instanceof IKalypsoFeatureTheme) )
      return false;

    /* Ignore invisible themes, so the user is able to tweak, which themes will be used */
    if( !theme.isVisible() )
      return false;

    final IKalypsoFeatureTheme featureTheme = (IKalypsoFeatureTheme) theme;
    final IFeatureRelation fp = featureTheme.getFeatureList();
    if( fp == null )
      return false;

    final Feature parentFeature = fp.getOwner();
    if( parentFeature == null )
      return false;

    final IRelationType targetType = fp.getPropertyType();
    if( targetType == null )
      return false;

    final IFeatureType targetFeatureType = targetType.getTargetFeatureType();
    if( GMLSchemaUtilities.substitutes( targetFeatureType, IProfileFeature.FEATURE_PROFILE ) )
      return true;

    if( GMLSchemaUtilities.substitutes( targetFeatureType, TuhhReachProfileSegment.QNAME_PROFILEREACHSEGMENT ) )
      return true;

    return false;
  }
}