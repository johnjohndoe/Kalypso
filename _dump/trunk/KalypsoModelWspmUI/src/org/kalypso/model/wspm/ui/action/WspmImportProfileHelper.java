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
package org.kalypso.model.wspm.ui.action;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.command.ICommand;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.gmleditor.ui.FeatureAssociationTypeElement;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEvent;

/**
 * @author Thomas Jung
 */
public class WspmImportProfileHelper
{
  /**
   * loads profiles into a given gml.
   */
  public static void loadIntoGml( final List<IProfil> profiles, final FeatureAssociationTypeElement fate, final CommandableWorkspace workspace ) throws Exception
  {
    final ICommand command = new ICommand()
    {
      private Feature[] m_addedFeatures = null;

      private FeatureList m_profileList = null;

      private GMLWorkspace m_workspace;

      private Feature m_waterFeature;

      public String getDescription( )
      {
        return "Profile importieren";
      }

      public boolean isUndoable( )
      {
        return true;
      }

      public void process( ) throws Exception
      {
        m_waterFeature = fate.getParentFeature();
        m_profileList = (FeatureList) m_waterFeature.getProperty( WspmWaterBody.QNAME_PROP_PROFILEMEMBER );

        final WspmWaterBody water = new WspmWaterBody( m_waterFeature );
        final List<Feature> newFeatureList = new ArrayList<Feature>();
        try
        {
          for( final IProfil profile : profiles )
          {
            final WspmProfile gmlProfile = water.createNewProfile();
            ProfileFeatureFactory.toFeature( profile, gmlProfile.getFeature() );

            newFeatureList.add( gmlProfile.getFeature() );
          }
        }
        catch( final GMLSchemaException e )
        {
          // should never happen, just log
          final IStatus status = StatusUtilities.statusFromThrowable( e );
          KalypsoModelWspmUIPlugin.getDefault().getLog().log( status );
        }
        finally
        {
          m_addedFeatures = newFeatureList.toArray( new Feature[newFeatureList.size()] );
          m_workspace = m_waterFeature.getWorkspace();
          final ModellEvent event = new FeatureStructureChangeModellEvent( m_workspace, m_waterFeature, m_addedFeatures, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD );
          m_workspace.fireModellEvent( event );
        }
      }

      @SuppressWarnings("unchecked")
      public void redo( ) throws Exception
      {
        m_profileList.addAll( Arrays.asList( m_addedFeatures ) );
        final ModellEvent event = new FeatureStructureChangeModellEvent( m_workspace, m_waterFeature, m_addedFeatures, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD );
        m_workspace.fireModellEvent( event );
      }

      @SuppressWarnings("unchecked")
      public void undo( ) throws Exception
      {
        m_profileList.removeAll( Arrays.asList( m_addedFeatures ) );
        final ModellEvent event = new FeatureStructureChangeModellEvent( m_workspace, m_waterFeature, m_addedFeatures, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE );
        m_workspace.fireModellEvent( event );
      }
    };

    if( workspace != null )
      workspace.postCommand( command );
  }

}
