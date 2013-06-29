package core;

/**
 * User: zola
 * Date: 25/06/2013
 */


import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

/**
 * User: zola
 * Date: 25/06/2013
 */
public class ParticleReaderTest {

//  @Test
//  public void testLoadPropertiesFile() throws Exception {
//    ParticleReader reader = new ParticleReader();
//    reader.loadDefaultProperties();
//    assertEquals("5", System.getProperty("particle.linkrange"));
//  }
//
//  @Test
//  public void testCustomPropertySet() throws Exception {
//
//    ParticleReader reader = new ParticleReader();
//    System.setProperty("particle.displacement", "30.0");
//    reader.loadDefaultProperties();
//    assertEquals("30.0", System.getProperty("particle.displacement"));
//
//  }

  @BeforeClass
  public void setUp() {
    // code that will be invoked when this test is instantiated
  }


  @Test(groups = { "fast" })
  public void aFastTest() {
    System.out.println("Fast test");
  }

}
