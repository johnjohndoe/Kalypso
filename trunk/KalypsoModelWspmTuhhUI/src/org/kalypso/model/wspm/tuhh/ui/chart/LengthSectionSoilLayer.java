package org.kalypso.model.wspm.tuhh.ui.chart;

import org.kalypso.chart.ext.observation.data.TupleResultDomainValueData;
import org.kalypso.chart.ext.observation.layer.TupleResultLineLayer;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

import de.openali.odysseus.chart.framework.model.layer.ILayerProvider;
import de.openali.odysseus.chart.framework.model.style.ILineStyle;
import de.openali.odysseus.chart.framework.model.style.IPointStyle;

public class LengthSectionSoilLayer extends TupleResultLineLayer
{
  /**
   * @see org.kalypso.chart.ext.observation.layer.TupleResultLineLayer#getTitle()
   */

  public LengthSectionSoilLayer( final ILayerProvider provider, final TupleResultDomainValueData< ? , ? > data, final ILineStyle lineStyle, final IPointStyle pointStyle )
  {

    super( provider, data, lineStyle, pointStyle );
  }

  @Override
  protected final String getTooltip( final int index )
  {
    final TupleResult tr = m_data.getObservation().getResult();
    final IRecord rec = tr.get( index );
    final int soilIndex = tr.indexOfComponent( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_GROUND );
    final int commentIndex = tr.indexOfComponent( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_TEXT );
    final int stationIndex = tr.indexOfComponent( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_STATION );
    final String targetOKComponentLabel = ComponentUtilities.getComponentLabel( tr.getComponent( soilIndex ) );
    final String stationComponentLabel = ComponentUtilities.getComponentLabel( tr.getComponent( stationIndex ) );
    final Double dn = ProfilUtil.getDoubleValueFor( soilIndex, rec );
    final Double ds = ProfilUtil.getDoubleValueFor( stationIndex, rec );
    final Object s = rec.getValue( commentIndex );
    if( s == null )
      return String.format( "%-12s %.4f%n%-12s %.4f", new Object[] { stationComponentLabel, ds, targetOKComponentLabel, dn } );//$NON-NLS-1$
    return String.format( "%-12s %.4f%n%-12s %.4f%n%s", new Object[] { stationComponentLabel, ds, targetOKComponentLabel, dn, s } );//$NON-NLS-1$

  }

}
