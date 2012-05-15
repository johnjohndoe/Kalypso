/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.ui.rrm.internal.results.view.base;

import org.eclipse.jface.resource.ImageDescriptor;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.UIRrmImages.DESCRIPTORS;

/**
 * @author Dirk Kuch
 */
public class KalypsoHydrologyResults
{
  // FIXME own parameter definitions and axes?!?

  public enum CATCHMENT_RESULT_TYPE
  {
    eTemperature("Temperatur", UIRrmImages.DESCRIPTORS.PARAMETER_TYPE_TEMPERATURE, "Temperatur.zml"),
    eNiederschlag("Niederschlag", UIRrmImages.DESCRIPTORS.PARAMETER_TYPE_RAINFALL, "Niederschlag.zml"),
    eSchneehoehe("Schneeh�he", UIRrmImages.DESCRIPTORS.PARAMETER_TYPE_SNOW_HEIGHT, null), // FIXME
    eGesamtTeilgebietsQ("Gesamtteilgebietsabfluss", UIRrmImages.DESCRIPTORS.PARAMETER_TYPE_DISCHARGE, "Gesamtabfluss.zml"),
    eOberflaechenQNatuerlich("Oberfl�chenabfluss, nat�rlich", UIRrmImages.DESCRIPTORS.PARAMETER_TYPE_DISCHARGE, "Oberflaechenabfluss(natuerlich).zml"),
    eOberflaechenQVersiegelt("Oberfl�chenabfluss, versiegelt", UIRrmImages.DESCRIPTORS.PARAMETER_TYPE_DISCHARGE, "Oberflaechenabfluss(versiegelt).zml"),
    eInterflow("Interflow", UIRrmImages.DESCRIPTORS.PARAMETER_TYPE_DISCHARGE, "Interflow.zml"),
    eBasisQ("Basisabfluss", UIRrmImages.DESCRIPTORS.PARAMETER_TYPE_DISCHARGE, "Basisabfluss.zml"),
    eGrundwasserQ("Grundwasserabfluss", UIRrmImages.DESCRIPTORS.PARAMETER_TYPE_DISCHARGE, "Grundwasserabfluss.zml"),
    eGrundwasserstand("Grundwasserstand", UIRrmImages.DESCRIPTORS.PARAMETER_TYPE_WATERLEVEL, "Grundwasserstand.zml"),
    eEvapotranspiration("Evapotranspiration", UIRrmImages.DESCRIPTORS.PARAMETER_TYPE_EVAPORATION, null); // FIXME

    private final String m_label;

    private final String m_fileName;

    private final DESCRIPTORS m_image;

    CATCHMENT_RESULT_TYPE( final String label, final UIRrmImages.DESCRIPTORS image, final String fileName )
    {
      m_label = label;
      m_image = image;
      m_fileName = fileName;
    }

    public String getLabel( )
    {
      return m_label;
    }

    @Override
    public String toString( )
    {
      return getLabel();
    }

    public ImageDescriptor getImage( )
    {
      return KalypsoUIRRMPlugin.getDefault().getImageProvider().getImageDescriptor( m_image );
    }

    public String getFileName( )
    {
      return m_fileName;
    }

  }

  public enum NODE_RESULT_TYPE
  {
    eGesamtknotenAbfluss("Gesamtknotenabfluss", UIRrmImages.DESCRIPTORS.PARAMETER_TYPE_DISCHARGE, "Gesamtabfluss.zml");

    private final String m_label;

    private final String m_fileName;

    private final DESCRIPTORS m_image;

    NODE_RESULT_TYPE( final String label, final UIRrmImages.DESCRIPTORS image, final String fileName )
    {
      m_label = label;
      m_image = image;
      m_fileName = fileName;
    }

    public String getLabel( )
    {
      return m_label;
    }

    public ImageDescriptor getImage( )
    {
      return KalypsoUIRRMPlugin.getDefault().getImageProvider().getImageDescriptor( m_image );
    }

    @Override
    public String toString( )
    {
      return getLabel();
    }

    public String getFileName( )
    {
      return m_fileName;
    }
  }

  public enum STORAGE_RESULT_TYPE
  {
    eFuellvolumen("F�llvolumen", UIRrmImages.DESCRIPTORS.PARAMETER_TYPE_VOLUME, "Fuellvolumen.zml"),
    eSpeicherUeberlauf("Specher�berlauf", UIRrmImages.DESCRIPTORS.PARAMETER_TYPE_DISCHARGE, "Speicherueberlauf.zml");

    private final String m_label;

    private final String m_fileName;

    private final DESCRIPTORS m_image;

    STORAGE_RESULT_TYPE( final String label, final UIRrmImages.DESCRIPTORS image, final String fileName )
    {
      m_label = label;
      m_image = image;
      m_fileName = fileName;
    }

    public String getLabel( )
    {
      return m_label;
    }

    @Override
    public String toString( )
    {
      return getLabel();
    }

    public ImageDescriptor getImage( )
    {
      return KalypsoUIRRMPlugin.getDefault().getImageProvider().getImageDescriptor( m_image );
    }

    public String getFileName( )
    {
      return m_fileName;
    }

  }
}
