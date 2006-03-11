/*--------------- Kalypso-Header ------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 --------------------------------------------------------------------------*/

package org.kalypso.ogc.gml.selection;

import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author belger
 */
public final class EasyFeatureWrapper
{
  /*
   * TODO !! remove this shit, refactor the featureAPI so that this is not necessary, PLEASE !!!!
   */

  private final CommandableWorkspace m_workspace;

  private final Feature m_feature;

  private final Feature m_parentFeature;

  private final IRelationType m_parentFeatureProperty;

  public EasyFeatureWrapper( final CommandableWorkspace workspace, final Feature feature, final Feature parentFeature, final IRelationType parentFeatureProperty )
  {
    m_workspace = workspace;
    m_feature = feature;
    m_parentFeature = parentFeature;
    m_parentFeatureProperty = parentFeatureProperty;
  }

  public Feature getFeature( )
  {
    return m_feature;
  }

  public Feature getParentFeature( )
  {
    return m_parentFeature;
  }

  public IRelationType getParentFeatureProperty( )
  {
    return m_parentFeatureProperty;
  }

  public CommandableWorkspace getWorkspace( )
  {
    return m_workspace;
  }
}
