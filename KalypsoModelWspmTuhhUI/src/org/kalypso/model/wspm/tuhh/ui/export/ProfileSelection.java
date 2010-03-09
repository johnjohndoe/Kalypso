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
package org.kalypso.model.wspm.tuhh.ui.export;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.viewers.ISelection;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ui.editor.gmleditor.ui.FeatureAssociationTypeElement;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;

/**
 * Helper class that extracts profiles from a selection.
 * 
 * @author Gernot Belger
 */
public class ProfileSelection
{
  private final IFeatureSelection m_selection;

  private final List<Feature> m_foundProfiles = new ArrayList<Feature>();

  private final List<Feature> m_selectedProfiles = new ArrayList<Feature>();

  public ProfileSelection( final ISelection selection )
  {
    m_selection = selection instanceof IFeatureSelection ? (IFeatureSelection) selection : null;

    findProfiles();
  }

  private void findProfiles( )
  {
    if( m_selection == null )
      return;

    final Object fe = m_selection.getFirstElement();
    if( fe instanceof FeatureAssociationTypeElement )
      addFeatureAssociation( (FeatureAssociationTypeElement) fe );
    else if( fe instanceof IProfileFeature )
      addProfileFeature( (IProfileFeature) fe );
    else if( fe instanceof Feature )
      addFeature( (Feature) fe );
  }

  private void addProfileFeature( final IProfileFeature profile )
  {
    final IRelationType rt = (profile).getParentRelation();
    final Feature parentFeature = (profile).getOwner();
    m_selectedProfiles.addAll( m_selection.toList() );
    if( rt.isList() )
      m_foundProfiles.addAll( (FeatureList) parentFeature.getProperty( rt ) );
    else
      m_foundProfiles.add( (Feature) parentFeature.getProperty( rt ) );
  }

  private void addFeature( final Feature fe )
  {
    // FIXME: probably a reach: find children of feature that are profiles
    if( GMLSchemaUtilities.substitutes( fe.getFeatureType(), TuhhReach.QNAME_TUHH_REACH ) )
      addTuhhReach( new TuhhReach( fe ) );

// final IRelationType rt = (fe).getParentRelation();
// final Feature parentFeature = (fe).getOwner();
// m_selectedProfiles.addAll( m_selection.toList() );
// if( rt.isList() )
// m_foundProfiles.addAll( (FeatureList) parentFeature.getProperty( rt ) );
// else
// m_foundProfiles.add( (Feature) parentFeature.getProperty( rt ) );
  }

  private void addTuhhReach( final TuhhReach reach )
  {
    final TuhhReachProfileSegment[] segments = reach.getReachProfileSegments();
    for( final TuhhReachProfileSegment segment : segments )
    {
      m_foundProfiles.add( segment.getProfileMember() );
      m_selectedProfiles.add( segment.getProfileMember() );
    }
  }

  private void addFeatureAssociation( final FeatureAssociationTypeElement fate )
  {
    final IRelationType rt = fate.getAssociationTypeProperty();
    final Feature parentFeature = fate.getParentFeature();
    if( rt.isList() )
    {
      m_selectedProfiles.addAll( (FeatureList) parentFeature.getProperty( rt ) );
      m_foundProfiles.addAll( (FeatureList) parentFeature.getProperty( rt ) );
    }
    else
    {
      m_selectedProfiles.add( (Feature) parentFeature.getProperty( rt ) );
      m_foundProfiles.add( (Feature) parentFeature.getProperty( rt ) );
    }
  }

  public boolean hasProfiles( )
  {
    return m_foundProfiles.size() > 0;
  }

  public Feature[] getProfiles( )
  {
    return m_foundProfiles.toArray( new Feature[m_foundProfiles.size()] );
  }

  public Feature[] getSelectedProfiles( )
  {
    return m_selectedProfiles.toArray( new Feature[m_selectedProfiles.size()] );
  }

}
