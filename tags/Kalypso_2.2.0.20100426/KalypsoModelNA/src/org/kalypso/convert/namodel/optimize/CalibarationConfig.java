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
package org.kalypso.convert.namodel.optimize;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.optimize.transform.ParameterOptimizeContext;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author doemming
 */
public class CalibarationConfig
{

  private final List<ParameterOptimizeContext> m_contexts;

  public CalibarationConfig()
  {
    m_contexts = new ArrayList<ParameterOptimizeContext>();
  }

  public void addFromNAControl( Feature rootFeatureControl )
  {
    // Catchments
    final String queryBaseCatchment = FeatureHelper.getAsString( rootFeatureControl, "Catchments" ); //$NON-NLS-1$
    String[] xpathControl = new String[]
    {
        "CatchmentsBianf", //$NON-NLS-1$
        "CatchmentsFaktorRetobTetint", //$NON-NLS-1$
        "CatchmentsFaktn", //$NON-NLS-1$
        "CatchmentsFaktorAigw" }; //$NON-NLS-1$

    String[][] xpathModel = new String[][]
    {
        new String[]
//        { queryBaseCatchment + "/:bodenkorrekturmember/:bodenschichtkorrektur/:banf" },
        { queryBaseCatchment + "/:faktorBianf" }, //$NON-NLS-1$
        new String[]
        { queryBaseCatchment + "/:faktorRetobRetint" }, //$NON-NLS-1$
        new String[]
        { queryBaseCatchment + "/:faktn" }, //$NON-NLS-1$
        new String[]
        { queryBaseCatchment + "/:faktorAigw" } }; //$NON-NLS-1$

    generateAndAddContexts( rootFeatureControl, xpathModel, xpathControl );
    // KMChannels
    final String queryBaseKMChannel = FeatureHelper.getAsString( rootFeatureControl, "KMChannels" ); //$NON-NLS-1$
    final String[] propNamesII = new String[]
    {
        "KMChannelsFaktorRkf", //$NON-NLS-1$
        "KMChannelsFaktorRnf" }; //$NON-NLS-1$

    final String[][] queryKMChannels = new String[][]
    {
        new String[]
        { queryBaseKMChannel + "/:faktorRkf" }, //$NON-NLS-1$
        new String[]
        { queryBaseKMChannel + "/:faktorRnf" } }; //$NON-NLS-1$
    generateAndAddContexts( rootFeatureControl, queryKMChannels, propNamesII );
  }

  private void generateAndAddContexts( Feature rootFeatureControl, String[][] xpathModel, String[] xpathControl )
  {
    final int n = xpathControl.length;
    for( int i = 0; i < n; i++ )
    {
      final String value = FeatureHelper.getAsString( rootFeatureControl, xpathControl[i] );
      if( value == null || value.length() == 0 )
        return;
      final double initialValue = Double.parseDouble( value );
      final String[] xPaths = xpathModel[i];
      addContext( new ParameterOptimizeContext( initialValue, 1, 0, 2, ParameterOptimizeContext.MODE_DIRECT, xPaths ) );
    }
  }

  public ParameterOptimizeContext[] getCalContexts()
  {
    return m_contexts.toArray( new ParameterOptimizeContext[m_contexts.size()] );
  }

  public void addContext( ParameterOptimizeContext context )
  {
    m_contexts.add( context );
  }

}
