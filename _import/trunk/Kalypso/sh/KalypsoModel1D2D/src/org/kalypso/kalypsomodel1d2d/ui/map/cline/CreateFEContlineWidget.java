package org.kalypso.kalypsomodel1d2d.ui.map.cline;

import org.kalypso.kalypsomodel1d2d.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IBoundaryLine;

/**
 * @author Gernot Belger
 */
public class CreateFEContlineWidget extends RouteLineElementWidget<IBoundaryLine>//<IFE1D2DContinuityLine>//extends AbstractWidget
{
  public CreateFEContlineWidget( )
  {
    super(
        Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.CreateFEContlineWidget.0"),//$NON-NLS-1$ 
        Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.CreateFEContlineWidget.1"),//$NON-NLS-1$
        IBoundaryLine.class,//IFE1D2DContinuityLine.class,
        Kalypso1D2DSchemaConstants.WB1D2D_F_BOUNDARY_LINE//Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2DContinuityLine
        );
  }

}
