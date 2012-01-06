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
package org.kalypso.model.wspm.tuhh.core.profile.export.knauf;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Set;

import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmProject;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.AbstractKnaufProjectBean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA10Bean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA11Bean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA12Bean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA13Bean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA91Bean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA94Bean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA99Bean;

/**
 * First basic implementation of a Knauf based KalypsoWpm calculation - an later implementation will be analog to
 *  {@link org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation)
 * 
 * @author Dirk Kuch
 */
public class KnaufCalculation
{

  private final IProfileFeature[] m_profiles;

  public KnaufCalculation( final IProfileFeature[] profiles )
  {
    m_profiles = profiles;
  }

  public KnaufReach[] getReaches( )
  {
    return new KnaufReach[] { new KnaufReach( m_profiles ) };
  }

  public WspmProject getProject( )
  {
    for( final IProfileFeature profile : m_profiles )
    {
      final WspmWaterBody waterBody = profile.getWater();
      if( Objects.isNull( waterBody ) )
        continue;

      final WspmProject project = waterBody.getProject();
      if( Objects.isNotNull( project ) )
        return project;
    }

    return null;
  }

  public AbstractKnaufProjectBean[] toBeans( )
  {
    final Set<AbstractKnaufProjectBean> beans = new LinkedHashSet<>();
    Collections.addAll( beans, getHeader() );

    final KnaufReach[] reaches = getReaches();
    for( final KnaufReach reach : reaches )
    {
      Collections.addAll( beans, reach.toBeans() );
    }

    Collections.addAll( beans, getFooter() );

    return beans.toArray( new AbstractKnaufProjectBean[] {} );
  }

  private AbstractKnaufProjectBean[] getHeader( )
  {
    final Set<AbstractKnaufProjectBean> beans = new LinkedHashSet<>();
    beans.add( new KnaufSA10Bean( this ) );
    beans.add( new KnaufSA11Bean( this ) );
    beans.add( new KnaufSA12Bean( this ) );
    beans.add( new KnaufSA13Bean( this ) );

    return beans.toArray( new AbstractKnaufProjectBean[] {} );
  }

  private AbstractKnaufProjectBean[] getFooter( )
  {
    final Set<AbstractKnaufProjectBean> beans = new LinkedHashSet<>();
    beans.add( new KnaufSA91Bean( this ) );
    beans.add( new KnaufSA94Bean( this ) );
    beans.add( new KnaufSA99Bean() );

    return beans.toArray( new AbstractKnaufProjectBean[] {} );
  }
}
