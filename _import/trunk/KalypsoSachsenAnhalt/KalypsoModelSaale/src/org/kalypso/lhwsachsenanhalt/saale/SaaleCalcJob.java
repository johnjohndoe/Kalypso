package org.kalypso.lhwsachsenanhalt.saale;

import java.io.File;
import java.net.URL;

import org.kalypso.services.calculation.job.ICalcDataProvider;
import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.job.ICalcMonitor;
import org.kalypso.services.calculation.job.ICalcResultEater;
import org.kalypso.services.calculation.service.CalcJobServiceException;

/**
 * <p>
 * Der Rechenservice für das Bode-Modell
 * </p>
 * 
 * @author Thül
 */
public class SaaleCalcJob implements ICalcJob
{
  /**
   * @throws CalcJobServiceException
   * @see org.kalypso.services.calculation.job.ICalcJob#run(java.io.File,
   *      org.kalypso.services.calculation.job.ICalcDataProvider,
   *      org.kalypso.services.calculation.job.ICalcResultEater,
   *      org.kalypso.services.calculation.job.ICalcMonitor)
   */
  public void run( final File tmpdir, final ICalcDataProvider inputProvider,
      final ICalcResultEater resultEater, final ICalcMonitor monitor )
      throws CalcJobServiceException

  {
    // nix tun ist auch schön
  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#getSpezifikation()
   */
  public URL getSpezifikation()
  {
    return getClass().getResource( SaaleConst.CALCJOB_SPEC );
  }
}