/*--------------- Kalypso-Header --------------------------------------------------------------------

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

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.command;

import java.util.List;

import org.kalypso.commons.command.ICommand;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.feature.event.FeaturesChangedModellEvent;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.visitors.CloneFeatureVisitor;

/**
 * TODO: please comment
 * 
 * @author belger
 */
public class ModifyFeatureCommand implements ICommand
{
  private final Feature[] m_targetFEs;

  private final GMLWorkspace m_workspace;

  private final Feature m_srcFeature;

  private final IPropertyType[] m_ftps;

  /**
   * @param workspace
   * @param targetFeatures
   *            features to modify
   */
  public ModifyFeatureCommand( final GMLWorkspace workspace, final Feature srcFeature, final IPropertyType[] ftps, final Feature targetFeatures[] )
  {
    m_workspace = workspace;
    m_ftps = ftps;
    m_targetFEs = targetFeatures;
    m_srcFeature = srcFeature;
  }

  public ModifyFeatureCommand( final GMLWorkspace workspace, final Feature srcFeature, final IPropertyType[] ftps, final Feature targetFeature )
  {
    this( workspace, srcFeature, ftps, new Feature[] { targetFeature } );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  public boolean isUndoable( )
  {
    return false;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  public void process( ) throws Exception
  {
    for( final Feature targetFE : m_targetFEs )
    {
      try
      {
        if( targetFE == m_srcFeature )
          continue;

        // collect list of feature ids

        // 1. remove target // command
        // check features to remove // TODO UNDO....
        // new DeleteFeatureCommand(m_workspace,targetFE,m_);
        for( final IPropertyType ftp : m_ftps )
        {
          // final String propName = ftp.getName();
          if( ftp instanceof IRelationType ) // remove sub features of target
          {
            final IRelationType linkPT = (IRelationType) ftp;
            final Object value = targetFE.getProperty( linkPT );
            if( value == null ) // nothing to do
              continue;
            if( linkPT.isList() )
            {
              final List list = (List) value;
              while( !list.isEmpty() )
              {
                // TODO is nicht schön... mit der liste
                final Object object = list.get( 0 );
                if( object instanceof Feature )
                  m_workspace.removeLinkedAsCompositionFeature( targetFE, linkPT, (Feature) object );
                else
                  m_workspace.removeLinkedAsAggregationFeature( targetFE, linkPT, (String) object );
              }
            }
            else
            {
              if( value instanceof Feature )
                m_workspace.removeLinkedAsCompositionFeature( targetFE, linkPT, (Feature) value );
              else
                m_workspace.removeLinkedAsAggregationFeature( targetFE, linkPT, (String) value );
            }
          }
        }

        final CloneFeatureVisitor visitor = new CloneFeatureVisitor( m_workspace, targetFE, m_ftps );
        m_workspace.accept( visitor, m_srcFeature, FeatureVisitor.DEPTH_INFINITE, m_ftps );
        // 2. broken links

        // }

        // }
        // else
        // {
        // final Object value = FeatureHelper.cloneData( valueOriginal, propType );
        // final FeatureProperty property;
        // property = FeatureFactory.createFeatureProperty( propName, value );
        // targetFE.setProperty( property );
        // }
      }
      catch( final Exception e )
      {
        // ignore exception and copy next features property
      }
    }
    // }
    final FeatureList list = FeatureFactory.createFeatureList( null, null, m_targetFEs );
    final Feature[] fs = (Feature[]) list.toArray( new Feature[list.size()] );
    m_workspace.fireModellEvent( new FeaturesChangedModellEvent( m_workspace, fs ) );
    //
    m_workspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_workspace, m_targetFEs, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo( ) throws Exception
  {
    process();
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {
    // for( int i = 0; i < m_targetFEs.length; i++ )
    // {
    // for( Iterator iter = m_oldMap[i].entrySet().iterator(); iter.hasNext(); )
    // {
    // final Map.Entry entry = (Entry)iter.next();
    // final FeatureProperty property = FeatureFactory
    // .createFeatureProperty( (String)entry.getKey(), entry.getValue() );
    // m_targetFEs[i].setProperty( property );
    // }
    // }
    // FeatureList list = FeatureFactory.createFeatureList( null, null, m_targetFEs );
    // m_workspace.fireModellEvent( new FeaturesChangedModellEvent( m_workspace, list ) );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    return "Wert ändern";
  }
}
