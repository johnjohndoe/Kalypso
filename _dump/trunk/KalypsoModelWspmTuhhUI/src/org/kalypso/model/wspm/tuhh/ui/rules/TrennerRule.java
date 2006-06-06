package org.kalypso.model.wspm.tuhh.ui.rules;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilConstants;
import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfilDevider.DEVIDER_TYP;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.validator.AbstractValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;


/**
 * Trennflächen und Bordvollpunkte dürfen nur innerhalb der durchströmten
 * Bereiche liegen
 * 
 * @author belger
 */
public class TrennerRule extends AbstractValidatorRule
{
  public void validate( final IProfil profil,
      final IValidatorMarkerCollector collector ) throws CoreException
  {
    if( profil == null )
      return;

    final IProfilDevider db[] = profil.getDevider( DEVIDER_TYP.DURCHSTROEMTE );
    final IProfilDevider tf[] = profil.getDevider( DEVIDER_TYP.TRENNFLAECHE );
    final IProfilDevider bv[] = profil.getDevider( DEVIDER_TYP.BORDVOLL );

    if( tf != null )
      validatePosition( db, tf, profil, collector );
    if( bv != null )
      validatePosition( db, bv, profil, collector );

  }

  private void validatePosition( IProfilDevider[] db, IProfilDevider[] toValidate,
      final IProfil profil, final IValidatorMarkerCollector collector )
      throws CoreException
  {
    try
    {
      final double left = db[0].getPoint().getValueFor( POINT_PROPERTY.BREITE );
      final double right = db[1].getPoint().getValueFor( POINT_PROPERTY.BREITE );
      final double xleft = toValidate[0].getPoint().getValueFor( POINT_PROPERTY.BREITE );
      final double xright = toValidate[1].getPoint()
          .getValueFor( POINT_PROPERTY.BREITE );

      if( (xleft < left) || (xleft > right) )
      {
        collector.createProfilMarker( true, toValidate[0].getTyp().toString() + " ["
            + String.format( IProfilConstants.FMT_STATION,  xleft )
            + "] liegt außerhalb des Durchströmten Bereichs", "", profil.getPoints().indexOf(toValidate[0].getPoint()), "" );
      }
      if( (xright < left) || (xright > right) )
      {
        collector.createProfilMarker( true, toValidate[1].getTyp().toString() + " ["
            + String.format( IProfilConstants.FMT_STATION,  xright )
            + "] liegt außerhalb des Durchströmten Bereichs", "", profil.getPoints().indexOf(toValidate[1].getPoint()), "" );
      }
      if( toValidate[0].getPoint() == toValidate[1].getPoint() )
      {
        collector.createProfilMarker( true,"doppelte "+ toValidate[1].getTyp().toString() , "", profil.getPoints().indexOf(toValidate[1].getPoint()), "" );
      }
    }
    catch( ProfilDataException e )
    {
      e.printStackTrace();
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin
          .getDefault().getBundle().getSymbolicName(), 0, "Profilfehler", e ) );
    }

  }
}
