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
package org.kalypsodeegree_impl.model.feature.validation;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypsodeegree.KalypsoDeegreeExtensions;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.validation.IFeatureMarkerCollector;
import org.kalypsodeegree.model.feature.validation.IFeatureRule;

/**
 * Starting point for the feature validation framework.
 * 
 * @see org.kalypsodeegree.model.feature.validation.IFeatureRule
 * @author Gernot Belger
 */
public class FeatureValidator
{
  private IFeatureMarkerCollector m_collector;

  public FeatureValidator( final IFeatureMarkerCollector collector )
  {
    m_collector = collector;
  }

/**
 * TODO: progressmonitor
 */  
  public void validateFeature( final Feature feature ) throws CoreException
  {
    m_collector.reset();

    final String pluginId = PluginUtilities.id( KalypsoDeegreePlugin.getDefault() );
    final MultiStatus multiStatus = new MultiStatus( pluginId, -1, "Problems while validating a feature.", null );

    final IFeatureRule[] featureRules = KalypsoDeegreeExtensions.getFeatureRules( feature );

    System.out.println( "Validating feature..." );

    for( final IFeatureRule rule : featureRules )
    {
      System.out.print( "Rule '" + rule.getName() + "'" );
      try
      {
        rule.validate( feature, m_collector );
      }
      catch( Throwable e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        multiStatus.add( status );
        KalypsoDeegreePlugin.getDefault().getLog().log( status );
      }
    }

    if( multiStatus.isOK() )
      throw new CoreException( multiStatus );
  }
}
