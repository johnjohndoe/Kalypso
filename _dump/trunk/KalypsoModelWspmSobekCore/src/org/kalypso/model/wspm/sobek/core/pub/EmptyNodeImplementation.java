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
package org.kalypso.model.wspm.sobek.core.pub;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.core.interfaces.IEmptyNode;
import org.kalypso.model.wspm.sobek.core.interfaces.IModelMember;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.model.wspm.sobek.core.model.AbstractNode;
import org.kalypso.model.wspm.sobek.core.model.Branch;
import org.kalypso.model.wspm.sobek.core.sperrzone.ISperrzone;
import org.kalypso.model.wspm.sobek.core.sperrzone.ISperrzonenDistances;
import org.kalypso.model.wspm.sobek.core.sperrzone.Sperrzone;
import org.kalypso.ogc.gml.FeatureUtils;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;

/**
 * Objects of class EmptyNodeImplementation will be returned for all created nodes which this plugin doesn't recognize
 * 
 * @author Dirk Kuch
 */
public class EmptyNodeImplementation extends AbstractNode implements IEmptyNode
{
  private Sperrzone m_sperrzone = null;

  public EmptyNodeImplementation( final IModelMember model, final Feature node )
  {
    super( model, node );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#delete()
   */
  public void delete( ) throws Exception
  {
    FeatureUtils.deleteFeature( getModel().getWorkspace(), getFeature() );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#getType()
   */
  public TYPE getType( )
  {
    return null;
  }

  public IBranch getLinkToBranch( )
  {
    IPropertyType myProperty = null;
    final IPropertyType[] properties = getFeature().getFeatureType().getProperties();
    for( final IPropertyType propertyType : properties )
    {
      if( ISobekConstants.F_LN_LINKS_TO_BRANCH.equals( propertyType.getQName().getLocalPart() ) )
      {
        myProperty = propertyType;
        break;
      }

    }

    final Object objBranch = getFeature().getProperty( myProperty );
    final Feature feature = FeatureHelper.resolveLinkedFeature( getModel().getWorkspace(), objBranch );

    if( feature == null )
      return null;

    return new Branch( getModel(), feature );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#isEmpty()
   */
  public boolean isEmpty( )
  {
    final IBranch branch = getLinkToBranch();
    if( branch == null )
      return true;

    return false;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#getSperrzone()
   */
  public ISperrzone getSperrzone( )
  {

    if( m_sperrzone == null )
    {
      m_sperrzone = new Sperrzone( getFeature() );
      try
      {
        final IBranch branch = getLinkToBranch();

        final GM_Point location = getLocation();
        final Geometry jtsLocation = JTSAdapter.export( location );

        final QName name = getFeature().getFeatureType().getQName();

        if( ISobekConstants.QN_NOFDP_RETARDIN_BASIN_NODE.equals( name ) )
        {
          final Geometry buffer = jtsLocation.buffer( ISperrzonenDistances.RETARDING_BASIN_NODE );
          m_sperrzone.addSperrzone( branch, buffer );
        }
        else
        {
          final Geometry buffer = jtsLocation.buffer( ISperrzonenDistances.DEFAULT_NODE );
          m_sperrzone.addSperrzone( branch, buffer );
        }
      }
      catch( final GM_Exception e )
      {
        e.printStackTrace();
      }

    }

    return m_sperrzone;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IEmptyNode#getStructureType()
   */
  public STRUCTURE_TYPE getStructureType( )
  {
    QName qname = getFeature().getFeatureType().getQName();

    if( ISobekConstants.QN_NOFDP_POLDER_NODE.equals( qname ) )
      return STRUCTURE_TYPE.ePolder;
    else if( ISobekConstants.QN_NOFDP_RETARDIN_BASIN_NODE.equals( qname ) )
      return STRUCTURE_TYPE.eRetardingBasin;
    else if( ISobekConstants.QN_NOFDP_WEIR_NODE.equals( qname ) )
      return STRUCTURE_TYPE.eWeir;

    throw new IllegalStateException();
  }
}
