package org.kalypso.ui.rrm.internal.results.view.base;

import org.kalypso.model.hydrology.project.RrmCalculationResult;
import org.kalypso.ui.rrm.internal.results.view.base.KalypsoHydrologyResults.RRM_RESULT_TYPE;

public class RrmResultBean
{
  private final RrmCalculationResult m_calculation;

  private final RRM_RESULT_TYPE m_type;

  public RrmResultBean( final RrmCalculationResult calculation, final RRM_RESULT_TYPE type )
  {
    m_calculation = calculation;
    m_type = type;
  }
}