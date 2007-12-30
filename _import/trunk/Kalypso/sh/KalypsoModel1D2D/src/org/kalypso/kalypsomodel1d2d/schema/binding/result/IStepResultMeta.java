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
package org.kalypso.kalypsomodel1d2d.schema.binding.result;

import java.util.Date;

import javax.xml.namespace.QName;

import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;

/**
 * @author Thomas Jung
 * 
 */
public interface IStepResultMeta extends IResultMeta
{
  enum STEPTYPE
  {
    steady
    {
      /**
       * @see java.lang.Enum#toString()
       */
      @Override
      public String toString( )
      {
        return "stationärer Rechenlauf";
      }
    },
    qSteady
    {
      /**
       * @see java.lang.Enum#toString()
       */
      @Override
      public String toString( )
      {
        return "quasi stationärer Rechenschritt";
      }
    },
    unsteady
    {
      /**
       * @see java.lang.Enum#toString()
       */
      @Override
      public String toString( )
      {
        return "instationärer Rechenschritt";
      }
    },
    maximum
    {
      /**
       * @see java.lang.Enum#toString()
       */
      @Override
      public String toString( )
      {
        return "maximale Strömungsgrößen";
      }
    },
    error
    {
      /**
       * @see java.lang.Enum#toString()
       */
      @Override
      public String toString( )
      {
        return "Protokoll";
      }
    }
  }

  public static final QName QNAME = new QName( UrlCatalog1D2D.MODEL_1D2DResult_NS, "StepResultMeta" );

  public static final QName QNAME_PROP_STEP_TIME = new QName( UrlCatalog1D2D.MODEL_1D2DResult_NS, "stepTime" );

  public static final QName QNAME_PROP_STEP_NUMBER = new QName( UrlCatalog1D2D.MODEL_1D2DResult_NS, "stepNumber" );

  public static final QName QNAME_PROP_STEP_IS_RESTART = new QName( UrlCatalog1D2D.MODEL_1D2DResult_NS, "isRestart" );

  public static final QName QNAME_PROP_STEP_TYPE = new QName( UrlCatalog1D2D.MODEL_1D2DResult_NS, "type" );

  public void setStepTime( final Date stepTime );

  public Date getStepTime( );

  public void setStepNumber( final Integer stepNumber );

  public Integer getStepNumber( );

  public boolean isRestart( );

  public void setRestart( boolean setRestart );

  public STEPTYPE getStepType( );

  public void setStepType( STEPTYPE stepType );

}