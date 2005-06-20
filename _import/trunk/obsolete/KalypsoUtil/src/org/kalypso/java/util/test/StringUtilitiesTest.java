package org.kalypso.java.util.test;

import junit.framework.TestCase;

import org.kalypso.java.util.StringUtilities;

/**
 * StringUtilitiesTest
 * 
 * @author schlienger
 */
public class StringUtilitiesTest extends TestCase
{
  public void testSpanOverLines()
  {
    final String text = "When the Workbench is launched the first thing you see is a dialog that allows you to select where the workspace should be located. The workspace is the directory where your work will be stored. For now, just click OK to pick the default location. (You can also check the checkbox to prevent this question from being asked again.)";

    System.out.println( StringUtilities.spanOverLines( text, 30, true, StringUtilities.ALIGNMENT_LEFT ) );
  }
}
