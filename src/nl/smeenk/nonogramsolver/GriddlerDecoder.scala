package nl.smeenk.nonogramsolver

import java.io.{ File, FileOutputStream, IOException, InputStream, OutputStream };
import java.net.URL;

object GriddlerDecoder {
  private val key = "java.util.String".getBytes();

  def main(args: Array[String]): Unit = {
    try {
      val id = 11859;
      val url = new URL("http://www.griddlers.net/puzzles/gdata.bin?id=" + id);
      val fileOut = new File(id + ".zip");
      val in = url.openStream();
      val out = new FileOutputStream(fileOut);
      val buffer = new Array[Byte](8192);
      var keyOffset = 0;
      var bytesRead = in.read(buffer);
      while (bytesRead >= 0) {
        decode(buffer, bytesRead, keyOffset);
        keyOffset += bytesRead;
        out.write(buffer, 0, bytesRead);
        bytesRead = in.read(buffer);
      }
      out.close();
      in.close();
    } catch {
      case e: IOException => e.printStackTrace
    }
  }

  private def decode(data: Array[Byte], length: Int, keyOffset: Int): Unit = {
    List.range(0, length).map { index: Int =>
      val keyIndex = ((index + keyOffset) % (key.length - 1))
      data(index) = (data(index) ^ key(keyIndex)).asInstanceOf[Byte]
    }
  }
}
