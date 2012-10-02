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
package org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.init;

import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.base.KnaufProfileWrapper;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.AbstractKnaufProjectBean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA20Bean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA21Bean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA29Bean;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.IProfileBuilding;

/**
 * @author Dirk Kuch
 */
public class KnaufBeanInitializer
{

  public static final void doInitialize( final AbstractKnaufProjectBean bean )
  {
    if( bean instanceof KnaufSA20Bean )
    {
      KnaufSA20BeanInitializer.init( (KnaufSA20Bean) bean );
    }
    else if( bean instanceof KnaufSA21Bean )
    {
      KnaufSA21BeanInitializer.init( (KnaufSA21Bean) bean );
    }
    else if( bean instanceof KnaufSA29Bean )
    {
      KnaufSA29BeanInitializer.init( (KnaufSA29Bean) bean );
    }
    else
      throw new UnsupportedOperationException();
  }

  protected static final IProfileBuilding getBuilding( final KnaufProfileWrapper profile )
  {
    final IProfileObject[] objects = profile.getProfile().getProfileObjects();
    for( final IProfileObject object : objects )
    {
      if( object instanceof IProfileBuilding )
        return (IProfileBuilding) object;
    }

    return null;
  }
}