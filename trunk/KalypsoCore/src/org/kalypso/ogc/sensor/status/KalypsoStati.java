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
package org.kalypso.ogc.sensor.status;

/**
 * Kalypso Status Constants
 * 
 * @author schlienger
 */
public interface KalypsoStati
{
  // TODO check concept of kalypsostati
  // what is the policy when tupples with
  // different stati get merged ?
  // merge stati with bit-or (|) ?
  // is "BIT_OK | BIT_CHECK" possible or is it better to remove BIT_OK and
  // define OK-status as "no bits are set" ?

  /** Value is OK ( bin: 1 dec: 1 ) */
  public final static int BIT_OK = 0x01;

  /** Value has to be CHECKed ( bin: 10 dec: 2 ) */
  public final static int BIT_CHECK = 0x02;

  /** User input is REQUIRED for this value ( bin: 100 dec: 4 ) */
  public final static int BIT_REQUIRED = 0x04;

  /** Value has been MODIFIED by user ( bin: 1000 dec: 8 ) */
  public final static int BIT_USER_MODIFIED = 0x08;

  /** Value could not be computed or derived from another value ( bin: 10000 dec: 16 ) */
  public final static int BIT_DERIVATION_ERROR = 0x10;

  /** Value was derivated from another one (such as a WQ-Transformation for instance) ( bin: 100000 dec: 32 ) */
  public final static int BIT_DERIVATED = 0x20;

  /** convenience object for usermod status */
  public final static Integer STATUS_USERMOD = new Integer( BIT_USER_MODIFIED );

  /** convenience object for check status */
  public final static Integer STATUS_CHECK = new Integer( BIT_CHECK );

  /** convenience object for derivation-error status */
  public final static Integer STATUS_DERIVATION_ERROR = new Integer( BIT_DERIVATION_ERROR );

  /** convenience object for derivated-status */
  public final static Integer STATUS_DERIVATED = new Integer( BIT_DERIVATED );
}