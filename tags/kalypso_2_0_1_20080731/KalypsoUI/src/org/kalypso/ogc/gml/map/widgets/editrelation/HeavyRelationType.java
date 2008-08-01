package org.kalypso.ogc.gml.map.widgets.editrelation;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FindExistingHeavyRelationsFeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;

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

public class HeavyRelationType implements org.kalypso.ogc.gml.map.widgets.editrelation.IRelationType
{

  private RelationType m_relationType1;

  private RelationType m_relationType2;

  /**
   * @author doemming
   */
  public HeavyRelationType( IFeatureType ft1, IRelationType linkFTP1, IFeatureType ft2, IRelationType linkFTP2, IFeatureType ft3 )
  {
    m_relationType1 = new RelationType( ft1, linkFTP1, ft2 );
    m_relationType2 = new RelationType( ft2, linkFTP2, ft3 );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.editrelation.IRelationType#fitsTypes(org.kalypsodeegree.model.feature.IFeatureType,
   *      org.kalypsodeegree.model.feature.IFeatureType)
   */
  public boolean fitsTypes( IFeatureType f1, IFeatureType f2 )
  {
    if( f1 == null || f2 == null )
      return false;
    return m_relationType1.getSrcFT() == f1 && m_relationType2.getDestFT() == f2;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.editrelation.IRelationType#getFitProblems(org.kalypsodeegree.model.feature.GMLWorkspace,
   *      org.kalypsodeegree.model.feature.Feature, org.kalypsodeegree.model.feature.Feature, boolean)
   */
  public String getFitProblems( GMLWorkspace workspace, Feature f1, Feature f2, boolean isAddMode )
  {
    FindExistingHeavyRelationsFeatureVisitor visitor = new FindExistingHeavyRelationsFeatureVisitor( workspace, this );
    visitor.visit( f1 );
    boolean exists = visitor.relationExistsTo( f2 );
    if( !isAddMode )
      return exists ? null : Messages.getString("org.kalypso.ogc.gml.map.widgets.editrelation.HeavyRelationType.0"); //$NON-NLS-1$
    // is addmode:
    if( exists )
      return Messages.getString("org.kalypso.ogc.gml.map.widgets.editrelation.HeavyRelationType.1"); //$NON-NLS-1$
    return m_relationType1.getFitProblemsfromOccurency( f1, isAddMode );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.editrelation.IRelationType#getDestFT()
   */
  public IFeatureType getDestFT( )
  {
    return m_relationType2.getDestFT();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.editrelation.IRelationType#getSrcFT()
   */
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

  // private final IFeatureType m_bodyFT;
  //
  // private final FeatureAssociationTypeProperty m_destLinkFTP;
  //
  // /*
  //
  // public IFeatureType getBodyFT()
  // {
  // return m_bodyFT;
  // }
  //
  // public IFeatureType getDestFT()
  // {
  // return m_destFT;
  // }
  //
  // public FeatureAssociationTypeProperty getDestLinkFTP()
  // {
  // return m_destLinkFTP;
  // }
  //
  // public IFeatureType getSrcFT()
  // {
  // return m_srcFT;
  // }
  //
  // public boolean equals( Object obj )
  // {
  // if( obj == null || !( obj instanceof HeavyRelationType ) )
  // return false;
  // final HeavyRelationType other = (HeavyRelationType)obj;
  // return super.equals( obj ) && other.getBodyFT().equals( m_bodyFT )
  // && other.getDestLinkFTP().equals( m_destLinkFTP );
  // }
  //
  // public int hashCode()
  // {
  // return ( getSrcFT().getName() + getLink().getName() + m_bodyFT.getName()
  // + m_destLinkFTP.getName() + getDestFT().getName() ).hashCode();
  // }
}