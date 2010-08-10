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

import javax.xml.namespace.QName;

import org.kalypso.convert.namodel.schema.UrlCatalogNA;
import org.kalypso.model.hydrology.binding.NAModellControl;
import org.kalypso.optimize.transform.ParameterOptimizeContext;

/**
 * @author doemming
 */
public class CalibrationConfig
{
  private final List<ParameterOptimizeContext> m_contexts = new ArrayList<ParameterOptimizeContext>();

  private final NAModellControl m_naControl;

  public CalibrationConfig( final NAModellControl naModellControl )
  {
    m_naControl = naModellControl;

    generateContexts();
  }

  public ParameterOptimizeContext[] getCalContexts( )
  {
    return m_contexts.toArray( new ParameterOptimizeContext[m_contexts.size()] );
  }

  // FIXME: use binding class to access all properties!
  private void generateContexts( )
  {
    // REMARK: this code relies heavily on the fixed prefix of the rrm-namespace.
    final String prefix = "/" + UrlCatalogNA.PREFIX_RRM + ":";

    // Catchments
    final String catchmentsProp = m_naControl.getCatchments();
    final String queryBaseCatchment = catchmentsProp + prefix; //$NON-NLS-1$

    final QName[] optimizeCatchmentProperties = NAModellControl.getOptimizeCatchmentsProperties();

    final String[] xpathModel = new String[] { queryBaseCatchment + "bianf", //$NON-NLS-1$
        queryBaseCatchment + "faktorRetobRetint", //$NON-NLS-1$
        queryBaseCatchment + "faktn", //$NON-NLS-1$
        queryBaseCatchment + "faktorAigw" }; //$NON-NLS-1$

    generateAndAddContexts( xpathModel, optimizeCatchmentProperties );

    // KMChannels
    final String asString = m_naControl.getKMChannels();
    final String queryBaseKMChannel = asString + prefix; //$NON-NLS-1$

    final QName[] optimizeChannelsProperties = NAModellControl.getOptimizeChannelsProperties();

    final String[] queryKMChannels = new String[] { queryBaseKMChannel + "faktorRkf", //$NON-NLS-1$
        queryBaseKMChannel + "faktorRnf" }; //$NON-NLS-1$

    generateAndAddContexts( queryKMChannels, optimizeChannelsProperties );
  }

  private void generateAndAddContexts( final String[] xpathModel, final QName[] xpathControl )
  {
    final int n = xpathControl.length;
    for( int i = 0; i < n; i++ )
    {
      final Object value = m_naControl.getProperty( xpathControl[i] );
      if( !(value instanceof Double) )
        return;

      final double initialValue = (Double) value;

      final String xPaths = xpathModel[i];
      final ParameterOptimizeContext context = new ParameterOptimizeContext( initialValue, 1, 0, 2, ParameterOptimizeContext.MODE_DIRECT, new String[] { xPaths } );
      addContext( context );
    }
  }

  private void addContext( final ParameterOptimizeContext context )
  {
    m_contexts.add( context );
  }

}
