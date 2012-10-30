/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.pdb.gaf;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.swt.graphics.RGB;
import org.kalypso.contribs.eclipse.swt.ColorUtilities;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;

/**
 * @author Holger Albert
 */
public enum GafPointCode
{
  PA( 0, "6010", Messages.getString("GafPointCode_1"), "PA", "P", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
  PP( 1, "6000", Messages.getString("GafPointCode_5"), StringUtils.EMPTY, "P", StringUtils.EMPTY, true ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  LBOK( 2, "917", Messages.getString("GafPointCode_8"), "LBOK", "P", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
  LBUK( 3, "918", Messages.getString("GafPointCode_12"), "LBUK", "P", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
  PST( 4, "970", Messages.getString("GafPointCode_16"), StringUtils.EMPTY, "P", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  PBA( 5, StringUtils.EMPTY, Messages.getString("GafPointCode_18"), StringUtils.EMPTY, "P", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$
  PBFS( 6, StringUtils.EMPTY, Messages.getString("GafPointCode_20"), StringUtils.EMPTY, "P", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$
  PBE( 7, StringUtils.EMPTY, Messages.getString("GafPointCode_22"), StringUtils.EMPTY, "P", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$
  LDOK( 8, "6020", Messages.getString("GafPointCode_25"), StringUtils.EMPTY, "P", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  LDUK( 9, "6030", Messages.getString("GafPointCode_28"), StringUtils.EMPTY, "P", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  LU( 10, "6040", Messages.getString("GafPointCode_31"), "LU", "P", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
  FS( 11, "6050", Messages.getString("GafPointCode_35"), StringUtils.EMPTY, "P", "#00FFFF", false ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
  RU( 12, "6060", Messages.getString("GafPointCode_39"), "RU", "P", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
  RBUK( 13, "918", Messages.getString("GafPointCode_43"), "RBUK", "P", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
  RBOK( 14, "917", Messages.getString("GafPointCode_47"), "RBOK", "P", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
  RDUK( 15, "6070", Messages.getString("GafPointCode_51"), StringUtils.EMPTY, "P", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  RDOK( 16, "6080", Messages.getString("GafPointCode_54"), StringUtils.EMPTY, "P", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  PE( 17, "6090", Messages.getString("GafPointCode_57"), "PE", "P", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
  SOA( 18, "6100", Messages.getString("GafPointCode_61"), StringUtils.EMPTY, "S", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  SOP( 19, "6100", Messages.getString("GafPointCode_64"), StringUtils.EMPTY, "S", StringUtils.EMPTY, true ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  SOE( 20, "6100", Messages.getString("GafPointCode_67"), StringUtils.EMPTY, "S", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  WS( 21, "6200", Messages.getString("GafPointCode_70"), StringUtils.EMPTY, "W", StringUtils.EMPTY, true ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  SPP0_8( 22, "7000", Messages.getString("GafPointCode_73"), StringUtils.EMPTY, "A", StringUtils.EMPTY, true ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  UKAN( 23, "7101", Messages.getString("GafPointCode_76"), StringUtils.EMPTY, "UK", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  UKPP( 24, "7100", Messages.getString("GafPointCode_79"), StringUtils.EMPTY, "UK", StringUtils.EMPTY, true ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  UKBA( 25, "7100", Messages.getString("GafPointCode_82"), StringUtils.EMPTY, "UK", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  UKWP( 26, "7102", Messages.getString("GafPointCode_85"), StringUtils.EMPTY, "UK", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  UKBW( 27, "7102", Messages.getString("GafPointCode_88"), StringUtils.EMPTY, "UK", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  UKBP( 28, "7103", Messages.getString("GafPointCode_91"), StringUtils.EMPTY, "UK", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  UKBE( 29, "7104", Messages.getString("GafPointCode_94"), StringUtils.EMPTY, "UK", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  UKEN( 30, "7109", Messages.getString("GafPointCode_97"), StringUtils.EMPTY, "UK", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  KRUK( 31, StringUtils.EMPTY, Messages.getString("GafPointCode_99"), StringUtils.EMPTY, "K", StringUtils.EMPTY, true ), //$NON-NLS-1$ //$NON-NLS-2$
  KRFS( 32, StringUtils.EMPTY, Messages.getString("GafPointCode_101"), StringUtils.EMPTY, "K", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$
  EIUK( 33, StringUtils.EMPTY, Messages.getString("GafPointCode_103"), StringUtils.EMPTY, "EI", StringUtils.EMPTY, true ), //$NON-NLS-1$ //$NON-NLS-2$
  EIFS( 34, StringUtils.EMPTY, Messages.getString("GafPointCode_105"), StringUtils.EMPTY, "EI", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$
  MAUK( 35, StringUtils.EMPTY, Messages.getString("GafPointCode_107"), StringUtils.EMPTY, "MA", StringUtils.EMPTY, true ), //$NON-NLS-1$ //$NON-NLS-2$
  MAFS( 36, StringUtils.EMPTY, Messages.getString("GafPointCode_109"), StringUtils.EMPTY, "MA", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$
  ARUK( 37, StringUtils.EMPTY, Messages.getString("GafPointCode_111"), StringUtils.EMPTY, "AR", StringUtils.EMPTY, true ), //$NON-NLS-1$ //$NON-NLS-2$
  ARFS( 38, StringUtils.EMPTY, Messages.getString("GafPointCode_113"), StringUtils.EMPTY, "AR", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$
  ARLR( 39, StringUtils.EMPTY, Messages.getString("GafPointCode_115"), StringUtils.EMPTY, "AR", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$
  ARRR( 40, StringUtils.EMPTY, Messages.getString("GafPointCode_117"), StringUtils.EMPTY, "AR", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$
  HAUK( 41, StringUtils.EMPTY, Messages.getString("GafPointCode_119"), StringUtils.EMPTY, "HA", StringUtils.EMPTY, true ), //$NON-NLS-1$ //$NON-NLS-2$
  HAFS( 42, StringUtils.EMPTY, Messages.getString("GafPointCode_121"), StringUtils.EMPTY, "HA", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$
  HALR( 43, StringUtils.EMPTY, Messages.getString("GafPointCode_123"), StringUtils.EMPTY, "HA", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$
  HARR( 44, StringUtils.EMPTY, Messages.getString("GafPointCode_125"), StringUtils.EMPTY, "HA", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$
  OKAN( 45, "7201", Messages.getString("GafPointCode_128"), StringUtils.EMPTY, "OK", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  OKPP( 46, "7200", Messages.getString("GafPointCode_131"), StringUtils.EMPTY, "OK", StringUtils.EMPTY, true ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  OKBA( 47, "7200", Messages.getString("GafPointCode_134"), StringUtils.EMPTY, "OK", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  OKBW( 48, "7202", Messages.getString("GafPointCode_137"), StringUtils.EMPTY, "OK", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  OKBP( 49, "7203", Messages.getString("GafPointCode_140"), StringUtils.EMPTY, "OK", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  OKBE( 50, "7204", Messages.getString("GafPointCode_143"), StringUtils.EMPTY, "OK", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  OKEN( 51, "7209", Messages.getString("GafPointCode_146"), StringUtils.EMPTY, "OK", StringUtils.EMPTY, false ), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  PWSP( 52, "0", Messages.getString("GafPointCode_149"), StringUtils.EMPTY, "W", StringUtils.EMPTY, false ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

  private final int m_number;

  private final String m_dbCode;

  private final String m_description;

  private final String m_hyk;

  private final GafKind m_kind;

  private final RGB m_color;

  private final boolean m_isDefault;

  private GafPointCode( final int number, final String dbCode, final String description, final String hyk, final String kind, final String color, final boolean isDefault )
  {
    m_number = number;
    m_dbCode = dbCode;
    m_description = description;
    m_hyk = hyk;
    m_kind = GafKind.valueOf( kind );
    m_color = ColorUtilities.toRGBFromHTML( color );
    m_isDefault = isDefault;
  }

  public String getKey( )
  {
    return name().replace( '_', '-' );
  }

  public int getNumber( )
  {
    return m_number;
  }

  public String getDbCode( )
  {
    return m_dbCode;
  }

  public String getDescription( )
  {
    return m_description;
  }

  public String getHyk( )
  {
    return m_hyk;
  }

  public GafKind getKind( )
  {
    return m_kind;
  }

  public RGB getColor( )
  {
    return m_color;
  }

  public boolean isDefault( )
  {
    return m_isDefault;
  }

  @Override
  public String toString( )
  {
    return m_description;
  }
}