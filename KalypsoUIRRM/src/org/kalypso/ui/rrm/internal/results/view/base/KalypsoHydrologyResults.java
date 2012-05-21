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
  public enum RRM_RESULT_TYPE
  {
    eNode,
    eCatchment,
    eStorage;
  }

  public enum RRM_RESULT
  {
    nodeGesamtknotenAbfluss(
        "Gesamtknotenabfluss", UIRrmImages.DESCRIPTORS.PARAMETER_TYPE_DISCHARGE, UIRrmImages.DESCRIPTORS.PARAMETER_MISSING_TYPE_DISCHARGE, "Gesamtabfluss.zml", RRM_RESULT_TYPE.eNode), //$NON-NLS-2$

    catchmentTemperature("Temperatur", UIRrmImages.DESCRIPTORS.PARAMETER_TYPE_TEMPERATURE, null, "Temperatur.zml", RRM_RESULT_TYPE.eCatchment), //$NON-NLS-2$
    catchmentNiederschlag("Niederschlag", UIRrmImages.DESCRIPTORS.PARAMETER_TYPE_RAINFALL, null, "Niederschlag.zml", RRM_RESULT_TYPE.eCatchment), //$NON-NLS-2$
    catchmentSchneehoehe("Schneehöhe", UIRrmImages.DESCRIPTORS.PARAMETER_TYPE_SNOW_HEIGHT, null, null, RRM_RESULT_TYPE.eCatchment), //$NON-NLS-2$ // FIXME
    catchmentGesamtTeilgebietsQ("Gesamtteilgebietsabfluss", UIRrmImages.DESCRIPTORS.PARAMETER_TYPE_DISCHARGE, null, "Gesamtabfluss.zml", RRM_RESULT_TYPE.eCatchment), //$NON-NLS-2$
    catchmentOberflaechenQNatuerlich("Oberflächenabfluss, natürlich", UIRrmImages.DESCRIPTORS.PARAMETER_TYPE_DISCHARGE, null, "Oberflaechenabfluss(natuerlich).zml", RRM_RESULT_TYPE.eCatchment), //$NON-NLS-2$
    catchmentOberflaechenQVersiegelt("Oberflächenabfluss, versiegelt", UIRrmImages.DESCRIPTORS.PARAMETER_TYPE_DISCHARGE, null, "Oberflaechenabfluss(versiegelt).zml", RRM_RESULT_TYPE.eCatchment), //$NON-NLS-2$
    catchmentInterflow("Interflow", UIRrmImages.DESCRIPTORS.PARAMETER_TYPE_DISCHARGE, null, "Interflow.zml", RRM_RESULT_TYPE.eCatchment), //$NON-NLS-2$
    catchmentBasisQ("Basisabfluss", UIRrmImages.DESCRIPTORS.PARAMETER_TYPE_DISCHARGE, null, "Basisabfluss.zml", RRM_RESULT_TYPE.eCatchment), //$NON-NLS-2$
    catchmentGrundwasserQ("Grundwasserabfluss", UIRrmImages.DESCRIPTORS.PARAMETER_TYPE_DISCHARGE, null, "Grundwasserabfluss.zml", RRM_RESULT_TYPE.eCatchment), //$NON-NLS-2$
    catchmentGrundwasserstand("Grundwasserstand", UIRrmImages.DESCRIPTORS.PARAMETER_TYPE_WATERLEVEL, null, "Grundwasserstand.zml", RRM_RESULT_TYPE.eCatchment), //$NON-NLS-2$
    catchmentEvapotranspiration("Evapotranspiration", UIRrmImages.DESCRIPTORS.PARAMETER_TYPE_EVAPORATION, null, null, RRM_RESULT_TYPE.eCatchment), //$NON-NLS-2$ // FIXME

    storageFuellvolumen("Füllvolumen", UIRrmImages.DESCRIPTORS.PARAMETER_TYPE_VOLUME, null, "Fuellvolumen.zml", RRM_RESULT_TYPE.eStorage), //$NON-NLS-2$
    storageSpeicherUeberlauf("Specherüberlauf", UIRrmImages.DESCRIPTORS.PARAMETER_TYPE_DISCHARGE, null, "Speicherueberlauf.zml", RRM_RESULT_TYPE.eStorage); //$NON-NLS-2$

    private final String m_label;

    private final String m_fileName;

    private final DESCRIPTORS m_image;

    private final DESCRIPTORS m_missing;

    private final RRM_RESULT_TYPE m_type;

    RRM_RESULT( final String label, final UIRrmImages.DESCRIPTORS image, final UIRrmImages.DESCRIPTORS missing, final String fileName, final RRM_RESULT_TYPE type )
    {
      m_label = label;
      m_image = image;
      m_missing = missing;
      m_fileName = fileName;
      m_type = type;
    }

    public String getLabel( )
    {
      return m_label;
    }

    public ImageDescriptor getImage( )
    {
      return KalypsoUIRRMPlugin.getDefault().getImageProvider().getImageDescriptor( m_image );
    }

    public ImageDescriptor getMissingImage( )
    {
      return KalypsoUIRRMPlugin.getDefault().getImageProvider().getImageDescriptor( m_missing );
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

    public RRM_RESULT_TYPE getType( )
    {
      return m_type;
    }
  }

}
