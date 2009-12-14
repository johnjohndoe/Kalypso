package org.kalypso.model.wspm.tuhh.ui.chart;

import org.eclipse.swt.graphics.GC;
import org.kalypso.chart.ext.observation.data.TupleResultDomainValueData;
import org.kalypso.chart.ext.observation.layer.TupleResultLineLayer;

import de.openali.odysseus.chart.framework.model.style.ILineStyle;
import de.openali.odysseus.chart.framework.model.style.IPointStyle;

public class LengthSectionWeirLayer extends TupleResultLineLayer
{
  /**
   * @see org.kalypso.chart.ext.observation.layer.TupleResultLineLayer#getTitle()
   */

  public LengthSectionWeirLayer( final TupleResultDomainValueData< ? , ? > data, final ILineStyle lineStyle, final IPointStyle pointStyle )
  {
    super( data, lineStyle, pointStyle );

  }

  @Override
  public void paint( final GC gc )
  {
// if( m_data == null )
// return;
// m_data.open();
//
// final PointFigure pf = getPointFigure();
// final IPointStyle ps = pf.getStyle();
// final FullRectangleFigure rf = new FullRectangleFigure();
// rf.setStyle( new AreaStyle( new ColorFill( ps.getInlineColor() ), ps.getAlpha(), ps.getStroke(), true ) );
// for( int i = 0; i < m_data.getObservation().getResult().size(); i++ )
// {
// final Rectangle rect = getScreenRect( i );
// if( rect != null )
// {
// rf.setRectangle( rect );
// rf.paint( gc );
// }
// }
  }

 
}
