package org.kalypso.services.user;

import java.util.Arrays;

/**
 * @author belger
 */
public class UserServiceConstants
{
  public static final String RIGHT_PROGNOSE = "Vorhersage";
  public static final String RIGHT_EXPERT = "Experte";
  public static final String RIGHT_ADMIN = "Administration";
  
  // full list of rights
  private static final String[] RIGHTS = { RIGHT_PROGNOSE, RIGHT_EXPERT, RIGHT_ADMIN };
  
  // sort array because of call to binarySearch
  static
  {
    Arrays.sort( RIGHTS );
  }
  
  /**
   * Returns true if the given right is a valid kalypso user right.
   * 
   * @param right
   * @return true when rigth is valid
   */
  public static boolean isValidUserRight( final String right )
  {
    return Arrays.binarySearch( RIGHTS, right ) >= 0;
  }
}
