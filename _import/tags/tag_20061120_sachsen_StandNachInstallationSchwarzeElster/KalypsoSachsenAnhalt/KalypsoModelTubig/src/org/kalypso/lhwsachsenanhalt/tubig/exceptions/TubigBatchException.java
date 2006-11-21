/*
 * ---------------- FILE HEADER KALYPSO ------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestra�e 22 21073
 * Hamburg, Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: g.belger@bjoernsen.de m.schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------
 */
package org.kalypso.lhwsachsenanhalt.tubig.exceptions;

/**
 * @author Th�l
 * @see org.eclipse.core.runtime.IStatus f�r Konstanten
 */
public class TubigBatchException extends Exception
{
  // Konstanten vergl. org.eclipse.core.runtime.IStatus
  public static final int STATUS_OK = 0;

  public static final int STATUS_INFO = 0x01;

  public static final int STATUS_WARNING = 0x02;

  public static final int STATUS_ERROR = 0x04;

  public static final int STATUS_CANCEL = 0x08;

  private final int m_status;

  private final String m_finishText;

  public TubigBatchException( final int iStatus, final String sFinishText )
  {
    this( null, null, iStatus, sFinishText );
  }

  public TubigBatchException( final String message, final int iStatus, final String sFinishText )
  {
    this( message, null, iStatus, sFinishText );
  }

  public TubigBatchException( final Throwable cause, final int iStatus, final String sFinishText )
  {
    this( null, cause, iStatus, sFinishText );
  }

  public TubigBatchException( final String message, final Throwable cause, final int iStatus, final String sFinishText )
  {
    super( message, cause );

    m_status = iStatus;
    m_finishText = sFinishText;
  }

  public int getStatus()
  {
    return m_status;
  }

  public String getFinishText()
  {
    return m_finishText;
  }
}