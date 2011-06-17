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
package org.kalypso.ui.rrm.internal.map.editRelation;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.commons.command.ICommand;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ui.editor.gmleditor.command.AddHeavyRelationshipCommand;
import org.kalypso.ui.editor.gmleditor.command.RemoveHeavyRelationCommand;
import org.kalypso.ui.rrm.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author doemming
 */
public class HeavyRelationType implements IEditRelationType
{
  private final LightRelationType m_relationType1;

  private final LightRelationType m_relationType2;

  public HeavyRelationType( final IFeatureType ft1, final IRelationType linkFTP1, final IFeatureType ft2, final IRelationType linkFTP2, final IFeatureType ft3 )
  {
    m_relationType1 = new LightRelationType( ft1, linkFTP1, ft2 );
    m_relationType2 = new LightRelationType( ft2, linkFTP2, ft3 );
  }

  @Override
  public String getFitProblems( final Feature f1, final Feature f2, final EditRelationMode mode )
  {
    final Feature[][] existingRelations = findExistingRelations( f1 );
    final Feature[] featureRelation = findRelation( existingRelations, f2 );
    final boolean exists = featureRelation != null;

    switch( mode )
    {
      case ADD:
        if( exists )
          return Messages.getString( "org.kalypso.ogc.gml.map.widgets.editrelation.HeavyRelationType.1" ); //$NON-NLS-1$
        else
        {
          final int maxOccurs = getLink1().getMaxOccurs();
          if( existingRelations.length >= maxOccurs )
            return "Maximum number of relations reached";
          else
            return null;
        }

      case REMOVE:
        if( exists )
          return null;
        else
          return Messages.getString( "org.kalypso.ogc.gml.map.widgets.editrelation.HeavyRelationType.0" );
    }

    throw new IllegalArgumentException();
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

  private Feature[][] findExistingRelations( final Feature sourceFeature )
  {
    final List<Feature[]> results = new ArrayList<Feature[]>();
    final IRelationType link1Name = getLink1();
    final IRelationType link2Name = getLink2();
    final Feature[] props1 = sourceFeature.getWorkspace().resolveLinks( sourceFeature, link1Name );
    for( final Feature feature1 : props1 )
    {
      if( feature1.getFeatureType().equals( getBodyFT() ) )
      {
        final Feature[] props2 = feature1.getWorkspace().resolveLinks( feature1, link2Name );
        for( final Feature feature2 : props2 )
        {
          if( feature2.getFeatureType().equals( getDestFT() ) )
            results.add( new Feature[] { sourceFeature, feature1, feature2 } );
        }
      }
    }

    return results.toArray( new Feature[results.size()][] );
  }

  private Feature[] findRelation( final Feature[][] existingRelations, final Feature f2 )
  {
    for( final Feature[] features : existingRelations )
    {
      if( features[2] == f2 )
        return features;
    }

    return null;
  }

  @Override
  public ICommand getRemoveCommand( final Feature sourceFeature, final Feature targetFeature )
  {
    final Feature[][] findExistingRelations = findExistingRelations( sourceFeature );
    final Feature[] relation = findRelation( findExistingRelations, targetFeature );
    if( relation == null )
      throw new IllegalStateException();

    final GMLWorkspace workspace = sourceFeature.getWorkspace();

    return new RemoveHeavyRelationCommand( workspace, sourceFeature, getLink1(), relation[1], getLink2(), targetFeature );
  }

  @Override
  public ICommand getAddCommand( final Feature sourceFeature, final Feature targetFeature )
  {
    final GMLWorkspace workspace = sourceFeature.getWorkspace();
    return new AddHeavyRelationshipCommand( workspace, sourceFeature, getLink1(), getBodyFT(), getLink2(), targetFeature );
  }
}