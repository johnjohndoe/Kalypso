/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans;

import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.KnaufReach;

/**
 * @author Dirk Kuch
 */
public class KnaufSA16Bean extends AbstractKnaufProjectBean
{
  private final KnaufReach m_reach;

  private Double m_dhwMax = 2.0;

  private Double m_vfMax = 8.0;

  private Double m_hzvMax = 1.0;

  private Double m_faklhg = 5.0;

  private Double m_ffMax = 5000.0;

  private Double m_bbrMax = 50.0;

  private Double m_fakrhyd = 0.6;

  public KnaufSA16Bean( final KnaufReach reach )
  {
    m_reach = reach;
  }

  @Override
  public Integer getSatzart( )
  {
    return 16;
  }

  /**
   * DHWMAX, maximale Differenz zwischen den Wasserspiegelhöhen benachbarter Querprofile Default-Wert : DHWMAX = 2.0 m
   */
  public Double getDHWMax( )
  {
    return m_dhwMax;
  }

  public void setDHWMax( final Double dhwMax )
  {
    m_dhwMax = dhwMax;
  }

  /**
   * VFMAX, maximale Fließgeschwindigkeit im Hauptgerinne. Default-Wert : VFMAX = 8 m/s
   */
  public Double getVFMax( )
  {
    return m_vfMax;
  }

  public void setVFMax( final Double vfMax )
  {
    m_vfMax = vfMax;
  }

  /**
   * 19 - 26 F8.0 m HZVMAX, maximale örtliche Verlusthöhe. Default-Wert : HZVMAX = 1.0 m
   */
  public Double getHZVMax( )
  {
    return m_hzvMax;
  }

  public void setHZVMax( final Double hzvMax )
  {
    m_hzvMax = hzvMax;
  }

  /**
   * 27 - 34 F8.0 - FAKLHG, maximaler Abstand zum OW-Querprofil bei einem Fließwechsel (wenn HGRENZ festgestellt wurde)
   * , hier definiert als Faktor zur Profilbreite. <br>
   * Default-Wert : FAKLHG = 5.0 FAKLHG < 0 keine Warnhinweise bei HGRENZ
   */
  public Double getFAKLHG( )
  {
    return m_faklhg;
  }

  public void setFAKLHG( final Double faklhg )
  {
    m_faklhg = faklhg;
  }

  /**
   * FFMAX, max. Fliessquerschnitt im Hauptgerinne. Default-Wert : FFMAX = 5000 m2
   */
  public Double getFFMax( )
  {
    return m_ffMax;
  }

  public void setFFMax( final Double ffMax )
  {
    m_ffMax = ffMax;
  }

  /**
   * BBRMAX, max. Profilabstand bei Brücken. Default : BBRMAX = 50 m
   */
  public Double getBBRMax( )
  {
    return m_bbrMax;
  }

  public void setBBRMax( final Double bbrMax )
  {
    m_bbrMax = bbrMax;
  }

  /**
   * FAKRHYD zur Berechnung der maximal zulässigen Rauheit maxkst = FAKRHYD*RHYD(n) (nur bei NHYD > 3 )<br>
   * Default : FAKRYD= 0.6 (nach BWK [44] ) // Nach DVWK [28] Seite 3 : FAKRHYD = 0.45
   */
  public Double getFAKRHYD( )
  {
    return m_fakrhyd;
  }

  public void setFAKRHYD( final Double fakrhyd )
  {
    m_fakrhyd = fakrhyd;
  }
}