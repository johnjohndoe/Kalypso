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
package org.kalypso.model.wspm.tuhh.core.profile.export.knauf.printer;

import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.AbstractKnaufProjectBean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA10Bean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA11Bean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA12Bean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA13Bean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA14Bean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA15Bean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA16Bean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA20Bean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA21Bean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA29Bean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA30Bean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA40Bean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA91Bean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA94Bean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA99Bean;

/**
 * @author Dirk Kuch
 */
public final class KnaufPrinterFactory
{
  private KnaufPrinterFactory( )
  {

  }

  public static IKnaufPrinter getPrinter( final AbstractKnaufProjectBean bean )
  {
    if( bean instanceof KnaufSA10Bean )
    {
      return new KnaufSA10Printer( (KnaufSA10Bean) bean );
    }
    else if( bean instanceof KnaufSA11Bean )
    {
      return new KnaufSA11Printer( (KnaufSA11Bean) bean );
    }
    else if( bean instanceof KnaufSA12Bean )
    {
      return new KnaufSA12Printer( (KnaufSA12Bean) bean );
    }
    else if( bean instanceof KnaufSA13Bean )
    {
      return new KnaufSA13Printer( (KnaufSA13Bean) bean );
    }
    else if( bean instanceof KnaufSA14Bean )
    {
      return new KnaufSA14Printer( (KnaufSA14Bean) bean );
    }
    else if( bean instanceof KnaufSA15Bean )
    {
      return new KnaufSA15Printer( (KnaufSA15Bean) bean );
    }
    else if( bean instanceof KnaufSA16Bean )
    {
      return new KnaufSA16Printer( (KnaufSA16Bean) bean );
    }
    else if( bean instanceof KnaufSA20Bean )
    {
      return new KnaufSA20Printer( (KnaufSA20Bean) bean );
    }
    else if( bean instanceof KnaufSA21Bean )
    {
      return new KnaufSA21Printer( (KnaufSA21Bean) bean );
    }
    else if( bean instanceof KnaufSA29Bean )
    {
      return new KnaufSA29Printer( (KnaufSA29Bean) bean );
    }
    else if( bean instanceof KnaufSA30Bean )
    {
      return new KnaufSA30Printer( (KnaufSA30Bean) bean );
    }
    else if( bean instanceof KnaufSA40Bean )
    {
      return new KnaufSA40Printer( (KnaufSA40Bean) bean );
    }
    else if( bean instanceof KnaufSA91Bean )
    {
      return new KnaufSA91Printer( (KnaufSA91Bean) bean );
    }
    else if( bean instanceof KnaufSA94Bean )
    {
      return new KnaufSA94Printer( (KnaufSA94Bean) bean );
    }
    else if( bean instanceof KnaufSA99Bean )
    {
      return new KnaufSA99Printer( (KnaufSA99Bean) bean );
    }

    throw new IllegalStateException();
  }
}
