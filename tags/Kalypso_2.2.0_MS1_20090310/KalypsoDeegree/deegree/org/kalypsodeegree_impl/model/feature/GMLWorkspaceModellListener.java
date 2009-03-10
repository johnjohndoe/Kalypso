/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.model.feature;

import java.util.HashSet;
import java.util.Set;

import javax.xml.namespace.QName;

import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
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

/**
 * @author Gernot Belger
 */
public class GMLWorkspaceModellListener implements ModellEventListener, FeatureVisitor
{
  private final Set<IGmlWorkspaceListener> m_validators = new HashSet<IGmlWorkspaceListener>();

  private final Set<QName> m_handledQNames = new HashSet<QName>();

  private final GMLWorkspace m_workspace;

  public GMLWorkspaceModellListener( final GMLWorkspace workspace )
  {
    m_workspace = workspace;

    /* Add qname-independent listeners. */
    final IGmlWorkspaceListener[] listeners = KalypsoDeegreeExtensions.createGmlWorkspaceListeners( null );
    for( final IGmlWorkspaceListener listener : listeners )
      addListener( listener );

    /* Find listeners for qnames */
    m_workspace.accept( this, workspace.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    initQName( f.getFeatureType() );

    return true;
  }

  private void initQName( final IFeatureType featureType )
  {
    final IFeatureType[] substituts = GMLSchemaUtilities.getSubstituts( featureType, featureType.getGMLSchema(), true, true );
    for( final IFeatureType substFeatureType : substituts )
    {
      final QName substQname = substFeatureType.getQName();

      if( !m_handledQNames.contains( substQname ) )
      {
        m_handledQNames.add( substQname );

        final IGmlWorkspaceListener[] listeners = KalypsoDeegreeExtensions.createGmlWorkspaceListeners( substQname );
        for( final IGmlWorkspaceListener listener : listeners )
          addListener( listener );
      }
    }
  }

  private void addListener( final IGmlWorkspaceListener listener )
  {
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
          /* Check for qname of added feature */
          final IFeatureType featureType = feature.getFeatureType();
          initQName( featureType );

          /* Check for its properties */
          final IPropertyType[] properties = featureType.getProperties();
          for( final IPropertyType type : properties )
            if( type instanceof IRelationType )
            {
              final IRelationType rt = (IRelationType) type;
              initQName( rt.getTargetFeatureType() );

              /* Check for current values qname */
            }
        }
      }
    }

    // simply inform all listeners of this workspace
    final IGmlWorkspaceListener[] ls = m_validators.toArray( new IGmlWorkspaceListener[m_validators.size()] );
    for( final IGmlWorkspaceListener validator : ls )
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
