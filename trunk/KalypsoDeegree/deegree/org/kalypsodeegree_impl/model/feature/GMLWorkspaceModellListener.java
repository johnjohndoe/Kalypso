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
package org.kalypsodeegree_impl.model.feature;

import java.util.HashSet;
import java.util.Set;

import javax.xml.namespace.QName;

import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.KalypsoDeegreeExtensions;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IGmlWorkspaceListener;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree_impl.model.feature.visitors.IndexFeaturesVisitor;

/**
 * @author Gernot Belger
 */
public class GMLWorkspaceModellListener implements ModellEventListener, FeatureVisitor
{
  private final Set<IGmlWorkspaceListener> m_validators = new HashSet<IGmlWorkspaceListener>();

  private final GMLWorkspace m_workspace;

  public GMLWorkspaceModellListener( final GMLWorkspace workspace )
  {
    m_workspace = workspace;

    /* Add qname-independent listeners. */
    final IGmlWorkspaceListener[] listeners = KalypsoDeegreeExtensions.getGmlWorkspaceListeners( null );
    for( final IGmlWorkspaceListener listener : listeners )
      addListener( listener );

    /* Find listeners for qnames */
    m_workspace.accept( this, workspace.getRootFeature(), IndexFeaturesVisitor.DEPTH_INFINITE );
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    final QName qname = f.getFeatureType().getQName();
    initQName( qname );

    return true;
  }

  private void initQName( final QName qname )
  {
    final IGmlWorkspaceListener[] listeners = KalypsoDeegreeExtensions.getGmlWorkspaceListeners( qname );
    for( final IGmlWorkspaceListener listener : listeners )
      addListener( listener );
  }

  private void addListener( final IGmlWorkspaceListener listener )
  {
    if( m_validators.contains( listener ) )
      return;

    listener.init( m_workspace );
    m_validators.add( listener );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    if( modellEvent instanceof FeatureStructureChangeModellEvent )
    {
      final FeatureStructureChangeModellEvent fscm = (FeatureStructureChangeModellEvent) modellEvent;
      if( fscm.getChangeType() == FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD )
      {
        final Feature[] parentFeatures = fscm.getParentFeatures();
        for( final Feature feature : parentFeatures )
        {
          final IPropertyType[] properties = feature.getFeatureType().getProperties();
          for( final IPropertyType type : properties )
          {
            if( type instanceof IRelationType )
            {
              final IRelationType rt = (IRelationType) type;
              final QName qname = rt.getTargetFeatureType().getQName();
              initQName( qname );
            }
          }
        }
      }
    }

    // simply inform all listeners of this workspace
    for( final IGmlWorkspaceListener validator : m_validators )
    {
      try
      {
        validator.onModellChange( modellEvent );
      }
      catch( final Throwable t )
      {
        KalypsoDeegreePlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( t ) );
      }
    }
  }
}
