package org.kalypso.ogc.gml.map.widgets.editrelation;

import java.util.List;

import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.GMLWorkspace;

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

public class RelationType implements IRelationType
{

  protected final FeatureType m_srcFT;

  protected final FeatureType m_destFT;

  private final FeatureAssociationTypeProperty m_link;

  /*
   * 
   * @author doemming
   */
  public RelationType( FeatureType srcFT, FeatureAssociationTypeProperty link, FeatureType destFT )
  {
    m_srcFT = srcFT;
    m_destFT = destFT;
    m_link = link;
  }

  public boolean fitsTypes( FeatureType f1, FeatureType f2 )
  {
    if( f1 == null || f2 == null )
      return false;
    return f1 == m_srcFT && f2 == m_destFT;
  }

  public String getFitProblems( GMLWorkspace workspace, Feature f1, Feature f2, boolean isAddMode )
  {
    
   boolean exists = workspace.isExistingRelation(f1,f2,m_link.getName());
    
    if( !isAddMode )
      return exists ? null : "diese Relation existiert nicht";
    // add mode:
    else if( exists )
    {
      return "Relation ist bereits gesetzt.";
    }
    return getFitProblemsfromOccurency( f1, isAddMode );
  }

  //  private boolean isExistingRelation( Feature f1, Feature f2 )
  //  {
  //    if( f2.getFeatureType().getName().startsWith( "KMCh" ) )
  //    {
  //      System.out.println( "test" );
  //    }
  //    final Object property = f1.getProperty( m_link.getName() );
  //    if( property == null )
  //      return false;
  //    if( property instanceof List )
  //    {
  //      if( ( (List)property ).contains( f2 ) )
  //        return true;
  //      if( ( (List)property ).contains( f2.getId() ) )
  //        return true;
  //      return false;
  //    }
  //    if( property == f2 )
  //      return true;
  //    if( f2.getId().equals( property ) )
  //      return true;
  //    return false;
  //  }
  //
  public String getFitProblemsfromOccurency( Feature f1, boolean isAddMode )
  {
    final String propName = m_link.getName();
    final String lang = KalypsoGisPlugin.getDefault().getLang();
    final String ftLabel = f1.getFeatureType().getAnnotation( lang ).getLabel();
    final String linkLabel = m_link.getAnnotation( lang ).getLabel();

    final Object property = f1.getProperty( propName );
    int max = m_srcFT.getMaxOccurs( propName );
    //    int min = m_srcFT.getMinOccurs( propName );
    if( isAddMode )
    {
      switch( max )
      {
      case FeatureType.UNBOUND_OCCURENCY:
        return null;
      case 1:
        return property == null ? null : ftLabel + "." + linkLabel + " ist schon gesetzt";
      default:
        return ( (List)property ).size() + 1 < max ? null : ftLabel + "." + linkLabel
            + " besitzt schon maximale relationen (" + max + ")";
      }
    }
    // else remove mode:
    switch( max )
    {
    case 1:
      return property != null ? null : ftLabel + "." + linkLabel + " ist nicht gesetzt";
    default: // minOccurs should not be validated here.
      return ( (List)property ).size() > 0 ? null : ftLabel + "." + linkLabel
          + " ist nicht gesetzt";
    }
  }

  public String toString()
  {
    return m_srcFT.getName() + " > " + m_destFT.getName();
  }

  public FeatureType getDestFT()
  {
    return m_destFT;
  }

  public FeatureAssociationTypeProperty getLink()
  {
    return m_link;
  }

  public FeatureType getSrcFT()
  {
    return m_srcFT;
  }

  //  public boolean equals( Object obj )
  //  {
  //    if( obj == null || !( obj instanceof RelationType ) )
  //      return false;
  //    final RelationType other = (RelationType)obj;
  //    return ( other.getSrcFT().equals( m_srcFT ) && other.getDestFT().equals(
  // m_destFT ) && other
  //        .getLink().equals( m_link ) );
  //  }
  //
  //  public int hashCode()
  //  {
  //    return ( m_srcFT.getName() + m_link.getName() + m_destFT.getName()
  // ).hashCode();
  //  }
}