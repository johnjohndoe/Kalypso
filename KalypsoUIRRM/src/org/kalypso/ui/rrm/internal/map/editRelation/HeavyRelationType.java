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
package org.kalypso.ui.rrm.internal.map.editRelation;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ui.rrm.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author doemming
 */
public class HeavyRelationType implements org.kalypso.ui.rrm.internal.map.editRelation.IEditRelationType
{
  private final LightRelationType m_relationType1;

  private final LightRelationType m_relationType2;

  public HeavyRelationType( final IFeatureType ft1, final IRelationType linkFTP1, final IFeatureType ft2, final IRelationType linkFTP2, final IFeatureType ft3 )
  {
    m_relationType1 = new LightRelationType( ft1, linkFTP1, ft2 );
    m_relationType2 = new LightRelationType( ft2, linkFTP2, ft3 );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.editrelation.IRelationType#fitsTypes(org.kalypsodeegree.model.feature.IFeatureType,
   *      org.kalypsodeegree.model.feature.IFeatureType)
   */
  @Override
  public boolean fitsTypes( final IFeatureType f1, final IFeatureType f2 )
  {
    return m_relationType1.getSrcFT().equals( f1 ) && m_relationType2.getDestFT().equals( f2 );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.editrelation.IRelationType#getFitProblems(org.kalypsodeegree.model.feature.GMLWorkspace,
   *      org.kalypsodeegree.model.feature.Feature, org.kalypsodeegree.model.feature.Feature, boolean)
   */
  @Override
  public String getFitProblems( final GMLWorkspace workspace, final Feature f1, final Feature f2, final boolean isAddMode )
  {
    final FindExistingHeavyRelationsFeatureVisitor visitor = new FindExistingHeavyRelationsFeatureVisitor( workspace, this );
    visitor.visit( f1 );
    final boolean exists = visitor.relationExistsTo( f2 );
    if( !isAddMode )
      return exists ? null : Messages.getString( "org.kalypso.ogc.gml.map.widgets.editrelation.HeavyRelationType.0" ); //$NON-NLS-1$
    // is addmode:
    if( exists )
      return Messages.getString( "org.kalypso.ogc.gml.map.widgets.editrelation.HeavyRelationType.1" ); //$NON-NLS-1$
    return m_relationType1.getFitProblemsfromOccurency( f1, isAddMode );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.editrelation.IRelationType#getDestFT()
   */
  @Override
  public IFeatureType getDestFT( )
  {
    return m_relationType2.getDestFT();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.editrelation.IRelationType#getSrcFT()
   */
  @Override
  public IFeatureType getSrcFT( )
  {
    return m_relationType1.getSrcFT();
  }

  public IRelationType getLink1( )
  {
    return m_relationType1.getLink();
  }

  public IRelationType getLink2( )
  {
    return m_relationType2.getLink();
  }

  public IFeatureType getBodyFT( )
  {
    return m_relationType1.getDestFT();
  }
}