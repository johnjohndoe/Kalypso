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
package org.kalypso.ui.wizards.imports.roughness;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.exception.IllegalFeatureState;
import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogModelSimulationBase;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author antanas
 */
public class RoughnessDatabase
{
  private final Feature m_Feature;

  private static final QName s_RootFT = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "RoughnessDatabase" );
  private static final QName s_RoughnessTypeFT = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "RoughnessType" );

  RoughnessDatabase( Feature feature )
  {
    if( feature == null )
      throw new IllegalArgumentException( "msg" );
    if( !feature.getFeatureType().getQName().equals( s_RootFT ) )
      throw new IllegalArgumentException( "second msg" );
    m_Feature = feature;
  }

  public double[] getCoefficients( ) throws IllegalFeatureState
  {
    Object coefs = m_Feature.getProperty( s_RoughnessTypeFT );
    if( coefs instanceof String )
    {
      String[] subStrings = ((String) coefs).split( " " );// "/s+");
      double doubles[] = new double[subStrings.length];
      for( int i = 0; i < subStrings.length; i++ )
      {
        doubles[i] = Double.parseDouble( subStrings[i] );
      }
      return doubles;
    }
    else
    {
      throw new IllegalFeatureState( m_Feature, s_RoughnessTypeFT, coefs );
    }
  }

}
