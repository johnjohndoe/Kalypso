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
package org.kalypso.convert.namodel.varymodel;

import java.util.ArrayList;
import java.util.List;

import org.deegree.model.feature.Feature;
import org.deegree_impl.model.feature.FeatureHelper;
import org.kalypso.optimize.transform.ParameterOptimizeContext;

/**
 * @author doemming
 */
public class CalibarationConfig
{

  private final List m_contexts;

  public CalibarationConfig()
  {
    m_contexts = new ArrayList();
  }

  public void addFromNAControl( Feature rootFeature )
  {
    // Catchments
    final String queryBaseCatchment = FeatureHelper.getAsString( rootFeature, "Catchments" );
    String[] propNamesI = new String[]
    {
        "CatchmentsBianf",
        "CatchmentsFaktorRetobTetint",
        "CatchmentsFaktn",
        "CatchmentsFaktorAigw" };

    String[][] queryCatchments = new String[][]
    {
        new String[]
//        { queryBaseCatchment + "//banf" },
        { queryBaseCatchment + "/faktorBianf" },
        new String[]
        { queryBaseCatchment + "/faktorRetobRetint" },
        new String[]
        { queryBaseCatchment + "/faktn" },
        new String[]
        { queryBaseCatchment + "/faktorAigw" } };

    generateAndAddContexts( rootFeature, queryCatchments, propNamesI );
    // KMChannels
    final String queryBaseKMChannel = FeatureHelper.getAsString( rootFeature, "KMChannels" );
    final String[] propNamesII = new String[]
    {
        "KMChannelsFaktorRkf",
        "KMChannelsFaktorRnf" };

    final String[][] queryKMChannels = new String[][]
    {
        new String[]
        { queryBaseKMChannel + "/faktorRkf" },
        new String[]
        { queryBaseKMChannel + "/faktorRnf" } };
    generateAndAddContexts( rootFeature, queryKMChannels, propNamesII );
  }

  private void generateAndAddContexts( Feature rootFeature, String[][] queryStrings,
      String[] propNames )
  {
    final int n = propNames.length;
    for( int i = 0; i < n; i++ )
    {
      final String value = FeatureHelper.getAsString( rootFeature, propNames[i] );
      if( value == null || value.length() == 0 )
        return;
      final double initialValue = Double.parseDouble( value );
      final String[] xPaths = queryStrings[i];
      addContext( new ParameterOptimizeContext( initialValue, 1, 0, 2, ParameterOptimizeContext.MODE_DIRECT, xPaths ) );
    }
  }

  public ParameterOptimizeContext[] getCalContexts()
  {
    return (ParameterOptimizeContext[])m_contexts.toArray( new ParameterOptimizeContext[m_contexts.size()] );
  }

  public void addContext( ParameterOptimizeContext context )
  {
    m_contexts.add( context );
  }

}

